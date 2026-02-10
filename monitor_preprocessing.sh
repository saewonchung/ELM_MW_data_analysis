#!/bin/bash
while true; do
    clear
    echo "============================================================"
    echo "ELM Preprocessing - Progress Monitor"
    echo "============================================================"
    echo ""
    
    # Count processed subjects
    if [ -d "ELM_preprocessed" ]; then
        processed=$(find ELM_preprocessed -name "*_desc-preproc_haemo.csv" | wc -l | tr -d ' ')
        subjects=$(ls -1 ELM_preprocessed 2>/dev/null | wc -l | tr -d ' ')
        echo "Subjects processed: $subjects"
        echo "Sessions processed: $processed / 118"
        echo ""
        
        # Show last 5 processed
        echo "Recent sessions:"
        find ELM_preprocessed -name "*_desc-preproc_haemo.csv" -type f -print0 | \
            xargs -0 ls -lt | head -5 | awk '{print "  "$9}' | \
            sed 's/.*sub-/  sub-/' | sed 's/_desc-preproc_haemo.csv//'
    else
        echo "No output yet..."
    fi
    
    echo ""
    echo "Press Ctrl+C to stop monitoring"
    sleep 10
done
