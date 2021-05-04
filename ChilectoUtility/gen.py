# -*- coding: utf-8 -*-
"""Generic utilities module contains commonly used functionalities.

This module includes some classes and functions for generic operations,
such as split one file into small files or select files in a folder.

Todo:
    * NA

"""

__author__ = "Weiwei"
__date__ = "12/29/2018"
__version__ = '1.0.0'

# create logger
import logging

gen_logger = logging.getLogger('cu.gen')


class FileSelect:
    """

    """
    def __init__(self, source_list):

        self.source_list = source_list

        self.file_list = []

        self.sub_file_list = []

        self.percentage = 100

        self.ext_include = []

    def reset(self):
        """

        """
        self.sub_file_list = self.file_list

    def get_file_list(self):
        """

        """
        from os import path

        for source in self.source_list:

            source_type = source[0]
            source_data = source[1]

            if source_type == 'file':

                if path.isfile(source_data):
                    gen_logger.info(f'Read file list in file: {source_data}.')

                    file_list = read_file(source_data, split_flag=True)

                else:
                    gen_logger.warning(
                        f'Source file {source_data} is not found.')

                    file_list = []

            elif source_type == 'dir':
                if path.isdir(source_data):
                    gen_logger.info(
                        f'Read file list in folder: {source_data}.')

                    file_list = list_file_in_dir(source_data,
                                                 recursive_flag=True)
                else:
                    gen_logger.warning(
                        f'Source folder {source_data} is not found.')

                    file_list = []

            elif source_type == 'list':
                file_list = source_data

            else:
                gen_logger.warning(f'Unsupported source type: {source_type}.')
                file_list = []

            self.file_list.extend(file_list)

        self.sub_file_list = self.file_list

        return

    def rand_select(self, percentage=-1):

        if percentage >= 0:
            self.percentage = percentage

        # if self.sub_flag:
        #     source_list = self.sub_file_list
        # else:
        #     source_list = self.file_list
        #     self.sub_flag = True

        self.sub_file_list = rand_file_select(self.sub_file_list,
                                              self.percentage)

        return

    def filter_include(self, ext_include):
        """Filter the file list based on the file extension name.

        Filter the input file list, return a file list only contains the
        files have the some extension name as file_ext_in.

        Args:

        Returns:

        """

        import os

        self.ext_include = ext_include

        if type(self.ext_include) is str:
            self.ext_include = [self.ext_include]

        out_file_list = []
        for file in self.sub_file_list:
            [_, file_ext] = os.path.splitext(file)
            if file_ext in self.ext_include:
                out_file_list.append(file)

        self.sub_file_list = out_file_list

        return out_file_list

    def select_by_num(self, num_list, idx1=0, idx2=-1):
        """Select files which contains int number in given number list.

        Args:
            num_list (list of int):
            idx1 (int): start index of the number
            idx2 (int): end index of the number

        Returns:
            list of str:

        """
        out_list = []

        for file in self.sub_file_list:
            number = name_2_int_idx(file, idx1, idx2)
            if number is None:
                gen_logger.info(f'Can not get int number in: {file}')
            else:
                if number in num_list:
                    out_list.append(file)

        out_list = sorted(out_list)

        self.sub_file_list = out_list

        return out_list

    def write_file(self, fname, full_list_flag=False):

        with open(fname, 'w') as fid:
            if full_list_flag:
                fid.write('\n'.join(self.file_list))
            else:
                fid.write('\n'.join(self.sub_file_list))


class CorpCounter(FileSelect):
    """Count the number of words in crops

    """

    def __init__(self, source_list, file_type, exc_line_start=None):
        import pandas as pd

        super().__init__(source_list)

        self.file_type = file_type

        self.word_count = []
        self.sub_word_count = []

        self.data_frame = pd.DataFrame()

        self.total_word = 0
        self.sub_total_word = 0

        self.get_file_list()

        if exc_line_start is None:
            self.exc_line_start = None
        elif type(exc_line_start) is str:
            self.exc_line_start = [exc_line_start]
        elif type(exc_line_start) is list:
            self.exc_line_start = exc_line_start
        else:
            gen_logger.error(f'Wrong exc_line_start data type')

    def count_one_file(self, file_name):

        if self.file_type == 'wpr':
            word_count = self.count_wpr_file(file_name)
        else:
            gen_logger.error(f'Wrong file type: {self.file_type}')

        return word_count

    def count_wpr_file(self, file_name):
        """Count the words in one word-per-row file.

        """
        from os import path
        exc_line_start = self.exc_line_start

        if path.isfile(file_name):
            fin = open(file_name, 'r', errors='replace')
            one_line = fin.readline()
            word_count = 0

            while one_line:
                exc_line_flag = False
                if exc_line_start is not None:
                    for check in exc_line_start:
                        if one_line.startswith(check):
                            exc_line_flag = True
                            break

                    if exc_line_flag:
                        one_line = fin.readline()
                        continue

                word_count += 1
                one_line = fin.readline()

            fin.close()

        else:
            gen_logger.error(f'Input file is not found,'
                             + f'file name: {file_name} ')

        return word_count

    def count_all_file(self):
        """

        """
        import tqdm

        n_file = len(self.file_list)
        self.word_count = [0] * n_file

        for idx in tqdm.tqdm(range(n_file)):
            file = self.file_list[idx]
            self.word_count[idx] = self.count_one_file(file)

        self.total_word = sum(self.word_count)

        return self.total_word

    def count_all_file_mp(self, np=4):

        import multiprocessing as mp
        import time

        t1 = time.time()
        p = mp.Pool(np)
        result = p.map(self.count_one_file, self.file_list)
        self.word_count = result
        t = time.time() - t1
        gen_logger.info(f'All files counted in {t:.1f} sec.')
        self.total_word = sum(self.word_count)

        self.count_sub_file_mp()

        return self.total_word

    def count_sub_file_mp(self, np=4):

        import multiprocessing as mp
        import time

        t1 = time.time()

        if len(self.word_count) == 0:
            p = mp.Pool(np)
            result = p.map(self.count_one_file, self.sub_file_list)
            self.sub_word_count = result
        else:
            self.sub_word_count = [0]*len(self.sub_file_list)
            for i, file in enumerate(self.sub_file_list):
                idx = self.file_list.index(file)
                self.sub_word_count[i] = self.word_count[idx]

        t = time.time() - t1
        gen_logger.info(f'All sub-files counted in {t:.1f} sec.')
        self.sub_total_word = sum(self.sub_word_count)

        return self.sub_total_word


def rand_file_select(file_list, percentage):
    """Randomly select certain percentage of files in the input file
    list.

    Args:
        file_list (list of str):
            a list in which each item is a string representing a file.
        percentage (int or float):
            the percentage of files wants to be selected. The number
            should between 0 and 100.

    Returns:
        list of str:
            a sub list of input file_list. The files are
            selected randomly using numpy.random.choice function. The
            number of file selected will be the rounded int number of
            percentage * len(file_list) / 100.

    Todo:
        * check the input percentage is in right range

    """
    from numpy import random
    from numpy import rint

    total_file = int(len(file_list))
    n_file = int(rint(total_file * percentage / 100))

    index_list = list(random.choice(total_file, n_file, replace=False))

    index_list.sort()

    rand_list = []

    for index in index_list:
        rand_list.append(file_list[index])

    rand_list.sort()

    return rand_list


def rand_file_in_dir(dir_in, percentage, out_file='', recursive_flag=False):
    """Randomly select certain percentage of files in a folder.

    Args:
        dir_in (str):
            input file folder
        percentage (int or float):
            the percentage of files wants to be selected. The number
            should between 0 and 100.
        out_file (str):
            output file name. If given, the output file list will also
            be writen in the output file.
        recursive_flag (bool):
            If TRUE, the folder will be processed recursively.

    Returns:
        list of str:
            a list of selected file names. The files are selected
            randomly using numpy.random.choice function. The number
            of file selected will be the rounded int number of
            percentage * len(file_list) / 100.

    Todo:
        * check the input percentage is in right range

    """

    file_list = list_file_in_dir(dir_in, recursive_flag)

    rand_list = rand_file_select(file_list, percentage)

    rand_list = sorted(rand_list)

    if out_file:
        write_file(out_file, rand_list)

    # fid = open(out_file, 'w')

    # for file in rand_list:
    #    fid.write(file + '\n')

    gen_logger.info(f'Files randomly selected in Folder {dir_in}.')

    return rand_list


def select_file_by_num(dir_in, num_list, out_file='',
                       idx1=0, idx2=-1, recursive_flag=False):
    """Select files which contains int number in given number list.

    Args:
        dir_in (str):
        num_list (list of int):
        out_file (str):
        idx1 (int): start index of the number
        idx2 (int): end index of the number
        recursive_flag (bool):


    Returns:
        list of str:

    """

    all_file = list_file_in_dir(dir_in, recursive_flag)

    out_list = []

    for file in all_file:
        number = name_2_int_idx(file, idx1, idx2)
        if number is None:
            gen_logger.info(f'Can not get int number in: {file}')
        else:
            if number in num_list:
                out_list.append(file)

    out_list = sorted(out_list)

    if out_file:
        with open(out_file, 'w') as fout:
            fout.write('\n'.join(out_list))

    return out_list


def write_file(file_name, str_in, sep='\n'):
    """Write the string in str_in into the file_name(str)

    If str_in is string, it will be written directly. If it's a list,
    sep.join(str_in) will be written to the file.

    """
    if type(str_in) is list:
        str_in = sep.join(str_in)

    with open(file_name, 'w') as fid:
        fid.write(str_in)

    return


def read_file(file_name, split_flag=False, sep='\n'):
    """Read text in file_name(str) and return in str_out"""
    with open(file_name) as fid:
        str_out = fid.read()

    if split_flag:
        str_out = str_out.split(sep)

    return str_out


def normalize_path(path_str):
    """Normalize the input path string

    Arg:
        path_str (str):
            string for a folder or file path

    Returns:
        str:
            normalized path by functions in os.path (expanduser,
            expandvars, normpath, normcase, abspath)
    """
    import os

    # normalize the dir_name
    path_str = os.path.expanduser(path_str)
    path_str = os.path.expandvars(path_str)
    path_str = os.path.normpath(path_str)
    path_str = os.path.normcase(path_str)
    path_str = os.path.abspath(path_str)

    return path_str


def list_file_in_dir(dir_name, recursive_flag=False):
    """List all files in dir_name(str). If recursive_flag is True,
    the folder will be processed recursively.
    """
    import os

    dir_name = normalize_path(dir_name)

    all_list = os.listdir(dir_name)

    # if dir_name[-1] == os.sep:
    #    dir_name = dir_name[:-1]

    file_list = []

    for item in all_list:
        item = dir_name + os.sep + item
        if os.path.isfile(item):
            file_list.append(item)
        else:
            if recursive_flag:

                file_list.extend(list_file_in_dir(item, True))

    file_list.sort()

    return file_list


def file_list_filter(file_list, file_ext_in):
    """Filter the file list based on the file extension name.

    Filter the input file list, return a file list only contains the
    files have the some extension name as file_ext_in.

    Args:
        file_list (list of str):
            a list of file names.
        file_ext_in (str):
            desired file extension name.

    Returns:
        (list of str):
            selected file name list

    """

    import os
    out_file_list = []
    for file in file_list:
        [_, file_ext] = os.path.splitext(file)
        if file_ext == file_ext_in:
            out_file_list.append(file)

    return out_file_list


def name_2_int_idx(file_name, idx1=0, idx2=-1):
    """Abstract int number in file name by index.

    Args:
        file_name (str):
            input file name or full path
        idx1 (int):
            start index of the number
        idx2 (int):
            end index of the number

    Returns:
        (int):
            the abstracted int number

    """
    import os
    file_name = os.path.basename(file_name)

    if idx2 == -1:
        idx2 = len(file_name)
    else:
        idx2 = idx2 + 1

    try:
        number = int(file_name[idx1:idx2])
    except:
        number = None
        print(file_name)
        print(file_name[idx1:idx2])

    return number


def load_vocab_dict(file_name, remove_pos=False, select_pos=[]):
    """Load the file contains the dictionary for vocabulary frequency.

    The file must be in the format of Python dictionary. The POS will
    be removed if the remove_pos is True.

    Args:
        file_name (str):
            the input file name.
        remove_pos (bool):
            True = remove the POS in the word string.
        select_pos (list of str):
            a list of string, each item is one POS want to be selected.

    Returns:
        vocab_dict (dic): a vocabulary frequency dictionary {word:freq}

    """
    import json
    with open(file_name) as fid:
        input_dict = json.load(fid)

    word_list = list(input_dict)
    vocab_dict = {}

    for word in word_list:
        temp_list = word.split('/')

        if len(temp_list) > 1:
            if len(select_pos) == 0 or temp_list[1] in select_pos:
                if remove_pos:
                    new_word = temp_list[0]
                else:
                    new_word = word

                if new_word in vocab_dict:
                    gen_logger.warning(f'Word {new_word} already exists.')

                    vocab_dict[new_word] = vocab_dict[new_word] + input_dict[word]

                else:
                    vocab_dict[new_word] = input_dict[word]
        else:
            vocab_dict[word] = input_dict[word]

    return vocab_dict


class FileSplit:
    """Class for splitting one data file into small files.

    This class contains attributes and methods related to process large
    data files. The main usage is to check and split one file into
    multiple files for further processing.

    """
    def __init__(self, file_name, output_dir):
        """Initialize one FileSplit object.

        Args:
            file_name (str):
                the name of the file needs to be processed.
            output_dir (str):
                output folder for split files.

        """
        import os

        #: str: the name of the file needs to be processed
        self.file_name = os.path.basename(file_name)
        #: str: the folder name of the target data file
        self.file_dir = os.path.dirname(file_name)
        #: str: output folder for split files
        self.output_dir = output_dir
        #: :obj:'file': file object to the target file
        self.fid = open(self.file_dir + '/' + self.file_name, errors='replace')
        #: :obj:'file': file object to the output file
        self.out_fid = open('fs_log.txt', 'w')
        #: bool: True for the output file has been opened
        self.out_file_open_flag = False
        #: str: tag string for the method of split the target file
        self.split_method = ''
        #: int: number of lines need to get from the target file
        self.num_of_line = 0
        #: str: the tag indicates the end of a data section
        self.end_tag = ''
        #: int: number of data sections in one split file
        self.num_of_tag = 0

        file_base_name, file_ext = os.path.splitext(self.file_name)
        #: str: the base name of target file
        self.file_base_name = file_base_name
        #: str: the extension name of the target file
        self.file_ext = file_ext
        #: str: the base name of output files
        self.output_base_name = self.file_base_name
        #: str: the extension name of the output files
        self.output_ext = self.file_ext
        #: int: number of split file generated
        self.num_of_file = 0

        self.fid.close()
        self.out_fid.close()

    def set_output_name(self, output_base_name, output_ext):
        self.output_base_name = output_base_name
        self.output_ext = output_ext

    def set_split_tag(self, end_tag, num_of_tag):
        self.split_method = 'tag'
        self.num_of_line = 0
        self.end_tag = end_tag
        self.num_of_tag = num_of_tag

    def open_out_file(self, file_index):
        if self.out_file_open_flag:
            self.out_fid.close()

        out_file_name = f'{self.output_dir}/{self.output_base_name}_{file_index:06}{self.output_ext}'

        self.out_fid = open(out_file_name, 'w')
        self.out_file_open_flag = True

    def split_tag(self):

        self.fid = open(self.file_dir + '/' + self.file_name, errors='replace')

        print(f'Splitting file {self.file_dir}/{self.file_name} by tag. In process ......')

        out_file_index = 1

        tag_count = 0

        eof_flag = False
        self.open_out_file(out_file_index)

        while not eof_flag:

            line = self.fid.readline()
            if line:

                self.out_fid.write(line)

                if line == self.end_tag:
                    tag_count = tag_count + 1

                if tag_count == self.num_of_tag:
                    out_file_index = out_file_index + 1
                    tag_count = 0
                    self.open_out_file(out_file_index)
            else:
                eof_flag = True

        self.out_fid.close()
        self.out_file_open_flag = False
        self.num_of_file = out_file_index
        self.fid.close()
        print(f'    {out_file_index} files are created\n')

    def check_file(self):

        self.fid = open(self.file_dir + '/' + self.file_name)
        line_num = 1
        eof_flag = False
        exception_log = open(self.file_dir + '/' + self.file_base_name + '.log', 'w')

        while not eof_flag:
            try:
                line = self.fid.readline()
            except Exception as e:
                line = 'Exception\n'
                print(str(e))
                exception_log.write(f'Line {line_num}: {str(e)}\n')

            if not line:
                eof_flag = True

            line_num = line_num + 1

            if line_num % 1000000 == 0:
                print(f'Line {line_num/1000000}m is finished')

        exception_log.close()

    def get_line_byte(self, line_list, out_file):

        out_fid = open(out_file, 'wb')

        self.fid = open(self.file_dir + '/' + self.file_name, 'rb')
        one_byte = self.fid.read(1)

        eof_flag = 0
        line_index = 1

        while not eof_flag:
            if line_index in line_list:
                out_fid.write(one_byte)
                if one_byte == b'\n':
                    line_list.remove(line_index)

            if one_byte == b'\n':
                line_index = line_index + 1

                if line_index % 1000000 == 0:
                    print(f'Line {line_index/1000000}m is finished')

            one_byte = self.fid.read(1)

            if not one_byte or len(line_list) == 0:
                eof_flag = 1

        self.fid.close()
        out_fid.close()

    def split_head(self, num_of_line=-1, output_file=''):

        self.fid = open(self.file_dir + '/' + self.file_name)

        if num_of_line > 0:
            self.num_of_line = num_of_line

        print(f'Grep the first {self.num_of_line} line of ' +
              f'{self.file_dir}/{self.file_name}')

        if output_file == '':
            output_file = self.file_dir + '/' + 'H_' + \
                str(self.num_of_line) + '_' + self.file_name

        fid_out = open(output_file, 'w')

        for i in range(self.num_of_line):
            fid_out.write(self.fid.readline())

        self.fid.close()
        fid_out.close()
        print(f'--->{output_file} is created.\n')


