
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Stomata detection

## preparation

please clone this repository to your local directory.

1.  open Stomata.RProj
2.  open run.R in folder “src”
3.  run
    [`set_up.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/set_up.R)
    to install necessary packages.
4.  create folder for data and results from line 6-7.
5.  put the folders contain .xml files under folder “data”, each folder
    would be one batch, for eaxmple: “T16L600”.

## read ground truth

6.  run
    [`read_xml.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/read_xml.R)
    from line 11-13 to read the files and generate basic results for
    each batch
7.  run
    [`stat_analysis.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/stat_analysis.R)
    line 18-20 to read the files and generate statistics for each
    stomata rows.
8.  run
    [`summarize_and_merge.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/summarize_and_merge.R)
    line 23-25 to generate summarize statistics for each picture.

## after`NTU`pipeline detection

To tackle the issues of same detected position of a stomata have
different class from the `NTU` pipeline.

### 9. Remove the duplicated.

1.  run line 29 to create the folder “result/Ntu”
2.  then placed the *no ground truth’s pictures* in “result/Ntu”
3.  run
    [`clean_ntu.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/check_ntu.R)
    from line 33-35 to remove the duplicated detected stomata that has
    lower confidence.
4.  merge.RDS will be the output removing the duplicated coordinates.

### 10. Compare the ground truth with the detection

For those picture that is both manually labelled and detected by the
`NTU`

1.  run
    [`check_ntu.R`](https://github.com/Illustratien/Stomata/blob/main/src/modules/clean_ntu.R)
    from line 38-40 to validate the detected data with ground truth

- note that only picture with the same picture name as ground truth when
  the detected stomata center is 3.2 microm to the ground truth.

## ❓ Issues/Problems/Errors

Please raise your question in
[Issues](https://github.com/HU-IGPS/Photosynthesis-Yichen-Model/issues)
and set a request in
[Project](https://github.com/orgs/HU-IGPS/projects/3)!

### Guideline for reporting error

> 1.  *Make sure your code is up to date (pull from github)*

2.  Describe the issue/problem/error clearly.

For example, to describe there is a issue of an unexpected mismatch
picture names from `clean_ntu.R`.

|        | input            | output           | script      |
|--------|------------------|------------------|-------------|
| format | res_noblurry.csv | detect_merge.RDS | clean_ntu.R |
| size   | 5760             | 480              | clean_ntu.R |

3.  identify the error: Is it an error? (Do you recieved error message?)

- If yes, Which code, which line, which error
- If it is inside a loop, can you identify the index inside the loop?
- see here more for
  [debug](https://ericlippert.com/2014/03/05/how-to-debug-small-programs/)

4.  Interpretation of the message: `Google` the error/ `Chatgpt`
5.  Reproduce the error: make sure it is
    [reproducible](https://stackoverflow.com/help/minimal-reproducible-example)
6.  Post on github/slack for help
