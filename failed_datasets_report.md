# Failed Datasets Analysis

## Summary
4 datasets failed preprocessing (3.4% failure rate)

## Failure Reasons

### Type 1: Missing Trigger "0.0" (3 datasets)
**Datasets:**
- 2025-05-19/2025-05-19_003 (sub-40)
- 2025-08-22/2025-08-22_002 (sub-053)
- 2025-10-07/2025-10-07_003 (sub-86_02)

**Issue:**
- These datasets have triggers: 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0
- Missing trigger: "0.0" (Video/TaskEnd)
- Current code expects all three triggers: 2.0, 3.0, 0.0

**Solution:**
Make trigger "0.0" optional in the preprocessing script.

### Type 2: Missing .nirs File (1 dataset)
**Dataset:**
- 2025-10-17/2025-10-17_001

**Issue:**
- No .nirs data file present (only config/calibration/tri files)
- Recording may have failed or been interrupted

**Solution:**
Skip this dataset - no data to process.

## Recommendation

Update preprocessing script to:
1. Make "0.0" trigger optional
2. Only rename triggers that exist in the data
3. Log warnings for missing expected triggers
