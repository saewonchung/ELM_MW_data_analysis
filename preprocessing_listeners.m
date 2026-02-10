%preprocessing script specifically for Pomona storytelling data - listeners

rawdir = '/Users/shannonburns/Library/CloudStorage/Box-Box/MICLab/projects/Storytelling_Study/data/listener_brain/raw';
outdir = '/Users/shannonburns/Library/CloudStorage/Box-Box/MICLab/projects/Storytelling_Study/data/listener_brain/preprocessed';
dataprefix = 'SL';
currdir=dir(strcat(rawdir,filesep,dataprefix,'*'));

for i=1:length(currdir)
    subj=currdir(i).name 
    subjdir=dir(strcat(rawdir,filesep,subj,filesep,dataprefix,'*'));

    for j=1:length(subjdir)
        scanname = subjdir(j).name;
        scanfolder = strcat(rawdir,filesep,subj,filesep,scanname);
        scanfile = dir(strcat(scanfolder,filesep,'*.snirf')).name;
        outpath = strcat(outdir,filesep,subj,filesep,scanname);
        if ~exist(outpath,'dir')
            mkdir(outpath)
        end

        %1) extract raw data values
        snirf = SnirfLoad(strcat(scanfolder,filesep,scanfile)); 
        load(strcat(subjfolder,filesep,probefile), 'probeInfo');
        d = snirf.data.dataTimeSeries;
        t = snirf.data.time;
        numchannels = size(d,2)/2;
        channelmask = ones(1,numchannels);
        SD = struct();
        SD.MeasList = snirf.data.cache.measurementListMatrix;
        SD.Lambda = snirf.probe.wavelengths;
        SD.MeasListAct = MeasList(:,3);
        SD.SrcPos = snirf.probe.sourcePos2D;
        SD.DetPos = snirf.probe.detectorPos2D;


        %2) channel quality assessment
        %signal strength SNR, noise/signal SD ratio, scalp coupling index,
        %and percentage w/o artifacts
        quality_report = zeros(4,numchannels);
        fs = abs(1/(t(2)-t(1)));
        tIncMan = ones(size(d,1),1);
        tMotion = 1; %Check for signal change indicative of a motion artifact over
                       %time range tMotion. Units of seconds.
        tMask = 1; %Mark data over +/- tMask seconds around the identified motion 
                   %artifact as a motion artifact. Units of seconds.
        STDEVthresh = 6;
        AMPthresh = 5;
        [~,tIncCh] = hmrMotionArtifactByChannel(d, fs, SD, tIncMan, tMotion, tMask, STDEVthresh, AMPthresh);
        FilterType = 1;
        FilterOrder = 5;
        [highb,higha] = MakeFilter(FilterType,FilterOrder,fs,0.5,'high');
        [lowb,lowa] = MakeFilter(FilterType,FilterOrder,fs,0.5,'low');
        dheart = filtfilt(highb,higha,d);
        dsignal = filtfilt(lowb, lowa, d);
        for c=1:numchannels
            SNR1 = mean(d(:,c)) / std(d(:,c));
            SNR2 = mean(d(:,c+numchannels)) / std(d(:,c+numchannels));
            quality_report(1,c) = mean([SNR1; SNR2]);

            ratio1 = std(dheart(:,c))/std(dsignal(:,c));
            ratio2 = std(dheart(:,c+numchannels))/std(dsignal(:,c+numchannels));
            quality_report(2,c) = mean([ratio1; ratio2]);

            SCI = corrcoef(dheart(:,c), dheart(:,(c+numchannels)));
            quality_report(3,c) = SCI(1,2);
            
            clean_percent1 = sum(tIncCh(:,c))/size(tIncCh,1);
            clean_percent2 = sum(tIncCh(:,c+numchannels))/size(tIncCh,1);
            quality_report(4,c) = mean([clean_percent1; clean_percent2]);
        end


        %3) remove motion spikes and discontinuities
        dod = hmrR_Intensity2OD_Nirs(d);

        %spline to remove discontinuities 
        p = 0.99;
        dspline = hmrR_MotionCorrectSpline_Nirs(dod, t, SD, tIncCh, p);

        %savitzky-golay filtering to remove spikes
        %https://www.spiedigitallibrary.org/journals/neurophotonics/volume-5/issue-01/015003/Motion-artifact-detection-and-correction-in-functional-near-infrared-spectroscopy/10.1117/1.NPh.5.1.015003.full?SSO=1
        K = 3; % polynomial order
        FrameSize_sec = 2;
        FrameSize_sec = round(FrameSize_sec * fs);
        if mod(FrameSize_sec,2)==0
            FrameSize_sec = FrameSize_sec  + 1;
        end
        dsg = sgolayfilt(dspline,K,FrameSize_sec);


        %4) convert to oxy with Modified Beer Lambert Law
        ppf = [6 6];
        dconverted = hmrR_OD2Conc_Nirs(dsg, SD, ppf);
        HbO = squeeze(dconverted(:,1,:));
        HbR = squeeze(dconverted(:,2,:));
        HbT = squeeze(dconverted(:,3,:));


        %5) set up aux data
        accelerometer1 = zeros(size(snirf.aux(1,1).dataTimeSeries,1), 6);
        accelerometer2 = zeros(size(snirf.aux(1,7).dataTimeSeries,1), 6);
        for ch=1:6
            accelerometer1(:,ch) = snirf.aux(1,ch).dataTimeSeries;
            accelerometer2(:,ch) = snirf.aux(1,(ch+6)).dataTimeSeries;
        end

        accelerometer1 = resample(accelerometer1, size(HbO,1), size(accelerometer1,1));
        accelerometer2 = resample(accelerometer2, size(HbO,1), size(accelerometer2,1));
        accelerometer = [accelerometer1, accelerometer2];


        %6) save data
        save(strcat(outpath,filesep,scanname,'_preprocessed.mat'),'HbO', 'HbR', 'HbT', 'accelerometer', 'quality_report','fs','t','SD', 'probeInfo');
            
    end
end



%removing bad channels, physio filtering, and z-scoring
prepdir = '/Users/shannonburns/Library/CloudStorage/Box-Box/MICLab/projects/Storytelling_Study/data/listener_brain/preprocessed';
dataprefix = 'SL';
currdir=dir(strcat(prepdir,filesep,dataprefix,'*'));

for i=1:length(currdir)
    subj=currdir(i).name; 
    subjdir=dir(strcat(prepdir,filesep,subj,filesep,dataprefix,'*'));

    for j=1:length(subjdir)
        scanname = subjdir(j).name;
        scanfile = strcat(prepdir,filesep,subj,filesep,scanname,filesep,scanname,'_preprocessed.mat');

        %1) load file and mark HbO as NaN based on scalp coupling index >
        %0.5
        load(scanfile)

        %2) filter with spatial PCA and detrending - only use if short
        %channels not available
        %coords = probeInfo.probes.coords_c3(1:size(HbO,2),:);
        %HbO_filtered = spatialPCFilter(HbO, coords);
        %HbO_filtered = detrend(HbO_filtered);

        bad_channels = find(quality_report(3,:)<0.5);
        HbO(:,bad_channels) = NaN;

        idx_short = [5,19,29,45,58,74,86,98]; %check your own short channel indices
        HbO_short = HbO(:,idx_short);
        HbO(:,idx_short) = [];
        quality_report(:, idx_short) = [];
        HbO_filtered(:,idx_short) = [];

        %2) remove nuissance regressors
        HbO_nuissance = [ones(size(HbO,1),1), HbO_short, accelerometer];
        HbO_nuissance(:,find(isnan(HbO_nuissance(1,:)))) = [];
        HbO_regressed = HbO;
        for ch=1:size(HbO,2)
            if ~isnan(HbO(1,ch))
                [~,~,HbO_r] = regress(HbO(:,ch),HbO_nuissance);
                HbO_regressed(:,ch) = HbO_r;
            end
        end

        %3) z-score
        HbO_z = zscore(HbO_regressed);
        HbO_short_z = zscore(HbO_short);

        %4) save to full data cell
        newfilename = strcat(prepdir,filesep,subj,filesep,scanname,filesep,scanname,'_02_HbOz.csv');
        writematrix(HbO_z, newfilename);
    end
end