# ChilectoUtility develop note
This folder contains python scripts for corpus pre-processing, Wordnet synset extraction and for the lectometric workflow.
## Issues

### Issue 001

1. Problem and required improvement

  1. *select_file_by_num* cannot handle the case that file name doesn't contain number
  2. file list related functions need more functionalities
     1. different type of input
     2. design a handler class

### Issue 002
1. Problem and required improvement
	1. `load_vocab_dict` needs modifications:
		1. input argument for POS selecting
		2. handle duplicated words after the POS is removed

## New functionalities

### Add 001:  wpr word count




