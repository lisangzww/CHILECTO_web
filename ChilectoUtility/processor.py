# -*- coding: utf-8 -*-
"""Processors for different corpus data file.

This module includes some classes and functions to pre-process some
corpus data files.

Todo:
    * NA

"""

__author__ = "Weiwei"
__date__ = "12/29/2018"
__version__ = '1.0.0'

# create logger
import logging
from .gen import FileSelect

processor_logger = logging.getLogger('cu.processor')


class SketchengineProcessor:
    """Class to process the sketch engine data file.

    The main function of this class is to add the 3rd column into the
    data file. If the 1st column of the data is in simplified Chinese,
    the 3rd column will be the same as the 1st one. If the 1st column
    of the data is in traditional Chinese, it will be converted to
    simplified Chinese using *hanziconv* package, and then saved to
    the 3rd column.
    """

    def __init__(self, data_dir, output_dir):
        """Initialize SketchengineProcessor object using:
            >>> sp = SketchengineProcessor(data_dir, output_dir)

        Args:
            data_dir (str):
                input data file folder
            output_dir (str):
                output folder for processed data

        """

        self.data_dir = data_dir
        """str: input data file folder"""

        self.output_dir = output_dir
        """str: output folder for processed data"""

        self._show_bar = True
        """bool: set to True to display the process bar"""

    @staticmethod
    def file_process(corpus_file_name, output_file):
        """Process one sketch engine data file, and save to output file

        Args:
            corpus_file_name (str):
                file name of the input corpus data file.
            output_file (str):
                file name of the output file.

        Returns:
            float:
                time in second cost to process the file.

        """

        import re
        import time
        from ChilectoUtility.gen import read_file
        from hanziconv import HanziConv

        if 'taiwan' in corpus_file_name:
            chinese_convert_flag = True
        else:
            chinese_convert_flag = False

        start_time = time.time()
        corpus_str = read_file(corpus_file_name)
        corpus_str_list = corpus_str.split('\n')

        fid_out = open(output_file, 'w')
        line_index = 1

        for one_line in corpus_str_list:
            re_match = re.search(r'<\S+>', one_line)
            if re_match:
                out_line = one_line
            else:
                re_match = re.search(r'([^ ]+)\s+(\S+)', one_line)
                if re_match:
                    if chinese_convert_flag:
                        add_str = HanziConv.toSimplified(re_match.group(1))
                    else:
                        add_str = re_match.group(1)

                    out_line = one_line + '\t' + add_str
                else:
                    out_line = one_line
                    # print(line_index, one_line)

            fid_out.write(out_line + '\n')
            line_index = line_index + 1

        fid_out.close()
        # print("%s: --- %6.1f seconds ---" % (corpus_file_name, time.time() - start_time))
        return time.time() - start_time

    @staticmethod
    def file_process_mp(argin):
        """File process function for multi thread process.

        Args:
            argin (list of str):
                a list contains two strings. The first one is the
                input file name, and the second one is the output
                file name.

        Returns:
            float:
                time in second cost to process the file.

        """
        output = SketchengineProcessor.file_process(argin[0], argin[1])
        return output

    def dir_process(self):
        """Process all files in data folder.

        This function process all files in the self.data_dir. The
        output file will be in the self.output_dir. Multiprocessing
        package is used for parallel computing.

        Returns:
            list of float:
                a list contains processing time in seconds for each
                file.

        """
        from ChilectoUtility import gen
        import os
        import tqdm

        print(f'Processing files in folder: {self.data_dir}')

        all_file = gen.list_file_in_dir(self.data_dir, True)

        argin_list = []

        for file in all_file:
            base_name = os.path.basename(file)

            output_file = self.output_dir + '/' + base_name

            argin_list.append([file, output_file])

        from multiprocessing import Pool

        p = Pool()

        if self._show_bar:
            result = list(
                            tqdm.tqdm(p.imap(self.file_process_mp, argin_list),
                                      total=len(argin_list))
                         )
        else:
            result = list(p.imap(self.file_process_mp, argin_list))

        p.close()
        p.join()

        print('    --> Processing finished.')

        return result


class LexicalEntry:
    """LexicalEntry read from wordnet XML data file.

    This class is designed to ues inside the WordNet class. The data
    file is normally in XML format. For example::
        <LexicalEntry id ='w228592'>
            <Lemma writtenForm='盟邦' partOfSpeech='n'/>
            <Sense id='w228592_08170978-n' synset='cmn-10-08170978-n'/>
            <Sense id='w228592_08171210-n' synset='cmn-10-08171210-n'/>
            <Sense id='w228592_08171592-n' synset='cmn-10-08171592-n'/>
        </LexicalEntry>

    """

    def __init__(self, entry_id, written_form, pos):
        """Initialize LexicalEntry object using:
            >>> le = LexicalEntry(entry_id, written_form, pos)
        Args:
            entry_id (int):
                the entry id
            written_form (str):
                written form of the entry
            pos (str):
                part of speech of the entry

        """

        self.id = entry_id
        """int: entry id number"""

        self.written_form = written_form
        """str: entry written form"""

        self.pos = pos
        """str: part of speech"""

        self.sense_dict = {}
        """dic: dictionary of all senses in the form {sense_id:synset}"""

        self.num_of_sense = 0
        """int: total number of senses of the entry"""

        self.freq = 0
        """int: freq of the written form in certain corpus"""

    def add_sense(self, sense_id, synset):
        """Add one sense to the sense dictionary.

        Args:
            sense_id (str):
                sense id
            synset (str):
                synset of the sense

        Returns:
            NULL

        """
        self.sense_dict[sense_id] = synset
        self.num_of_sense = len(list(self.sense_dict))

    def print_entry(self):
        """Print the entry on screen.

        """
        print(f'id={self.id}   {self.written_form}/{self.pos}  with {self.num_of_sense:d} senses, freq={self.freq:d}')
        for sense_id in list(self.sense_dict):
            print('%s -- %s' % (sense_id, self.sense_dict[sense_id]))

    def entry_string_t1(self, sep=', '):
        """Generate a multi-line string represents all senses and freq
        for the entry.

        Args:
            sep (str):
                the string used to separate data in the output string,
                the default value is ', '.

        Returns:
            str:
                a multi-line string, each line is in the form of
                sense_id+sep+writtenform+sep+freq

        """
        from hanziconv import HanziConv
        writtenform = HanziConv.toSimplified(self.written_form)
        out_str = ''
        for sense_id in list(self.sense_dict):
            one_line = f'{self.sense_dict[sense_id]}{sep}{writtenform}{sep}{self.freq}\n'
            out_str = out_str + one_line
        return out_str


class WordNet:
    """WordNet object represents wordnet data read from XML date file.

    Main functions of this class include:
        #. read lexical entries from XML file and stored them list
        #. read word frequency list from data file
        #. match the writtenform of lexical entries with the frequency
           list
        #. generated output file

    """

    def __init__(self, xml_file_name):
        """Initialize WordNet object using:
            >>> wn = WordNet(xml_file_name)

        Args:
            xml_file_name (str):
                input xml data file name

        """

        #: str: input xml data file name
        self.xml_file_name = xml_file_name
        #: list of LexicalEntry obj: all entries from the data file
        self.lexical_entry_list = []
        #: int: total number of entries
        self.num_of_entry = 0
        #: str: file name of the word frequency
        self.freq_file = ''
        #: list of int: freq order of all entries
        self.freq_order = []
        #: int: entries have freq lower than this will be neglected in the data frame
        self.freq_min = -1
        import pandas as pd
        #: PD data frame:
        self.data_frame = pd.DataFrame(data={'synset': [],
                                             'writtenform': [],
                                             'freq': []})
        #: list of str:
        self.writtenform_list = []

    def add_entry(self, one_entry):
        """Add one entry to the object entry list.

        If the written form of the input entry exits in the list, the
        sense_dict of the existing entry will be updated by the input
        entry. Otherwise, the input entry will be appended at the end
        of the entry list.

        Args:
            one_entry (class LexicalEntry):
                the new entry needs to be added

        Returns:
            NULL

        """

        try:
            entry_index = self.writtenform_list.index(one_entry.written_form)
        except ValueError:
            entry_index = -1

        if entry_index >= 0:
            self.lexical_entry_list[entry_index].sense_dict.update(one_entry.sense_dict)
        else:
            self.lexical_entry_list.append(one_entry)
            self.writtenform_list.append(one_entry.written_form)

    def read_entry(self, xml_file_name=''):
        """Read all lexical entries from the data file.

        Args:
            xml_file_name (str):
                data file name. If not given, the *obj.xml_file_name*
                file will be used.

        Returns:
            NULL

        """

        import xml.etree.ElementTree as ET
        from hanziconv import HanziConv as hz
        import tqdm

        if xml_file_name:
            self.xml_file_name = xml_file_name

        processor_logger.info(f'Reading entry file: {self.xml_file_name}')

        tree = ET.parse(self.xml_file_name)
        root = tree.getroot()

        for entry_xml in tqdm.tqdm(list(root.iter('LexicalEntry'))):
            if 'id' in entry_xml.attrib:
                entry_id = entry_xml.attrib['id']
            else:
                entry_id = 'NA'

            written_form = entry_xml[0].attrib['writtenForm']
            pos = entry_xml[0].attrib['partOfSpeech']

            entry = LexicalEntry(entry_id, written_form, pos)

            for sense in entry_xml.iter('Sense'):
                sense_id = sense.attrib['id']
                synset = sense.attrib['synset']

                entry.add_sense(sense_id, synset)

            entry.writtenform = hz.toSimplified(entry.written_form)

            self.add_entry(entry)

        self.num_of_entry = len(self.lexical_entry_list)

        processor_logger.info(f'{self.num_of_entry} entries imported.')

        self.freq_order = range(0, self.num_of_entry)

    def get_entry_freq(self, freq_list_file):
        """Load word frequency from file. Update the freq of each
        lexical entry in the WordNet object.

        Args:
            freq_list_file (str): data file name

        Returns:
            NULL

        """

        from ChilectoUtility.gen import load_vocab_dict

        self.freq_file = freq_list_file

        processor_logger.info(f'Reading freq list from {freq_list_file}')

        vocab_dict = load_vocab_dict(freq_list_file, remove_pos=True)

        for entry in self.lexical_entry_list:

            if entry.written_form in vocab_dict:
                entry.freq = vocab_dict[entry.written_form]
            else:
                entry.freq = 0

        # processor_logger.info(f'Reading freq list finished.')

    def sort_entry_freq(self):
        """Sort lexical entries by freq. The entry list will not be
        modified. The frequency order will be stored in
        self.freq_order

        Args:
            NULL

        Returns:
            NULL

        """
        import numpy
        all_freq = []
        for entry in self.lexical_entry_list:
            all_freq.append(entry.freq)

        freq_array = numpy.array(all_freq)
        sort_index = numpy.argsort(freq_array)
        self.freq_order = list(reversed(sort_index.tolist()))

    def write_to_file(self, file_name, sep=', ', order_by_freq=False):
        """Write all entries into the output file.

        The output string for each entry is generated by LexicalEntry
        class function entry_string_t1.

        Args:
            file_name (str):
                output file name
            sep (str):
                separation string needed for
                *LexicalEntry.entry_string_t1* function
            order_by_freq (bool):
                if TRUE, the output order of entries will be based on
                their freq

        Returns:
            NULL

        """

        if order_by_freq:
            index_list = self.freq_order
        else:
            index_list = range(0,self.num_of_entry)

        fid = open(file_name, 'w')

        head = sep.join(['synset', 'writtenform', 'freq'])
        fid.write(head+'\n')

        for i in index_list:
            out_str = self.lexical_entry_list[i].entry_string_t1(sep)
            fid.write(out_str)

        fid.close()

        processor_logger.info(f'Write entry data to {file_name}')

    def convert_to_dataframe(self, freq_min=-1):
        """Convert entry list into pandas data frame.

        The data frame will be stored as self.data_frame. In the data
        frame, entries with frequency lower than freq_min will be
        neglected.

        Args:
            freq_min (int):
                minimum freq of entries will be keep in the data frame

        Returns:
            NULL

        """
        import pandas as pd
        from tqdm import tqdm

        processor_logger.info(f'Convert entry list to dataframe, '
                              + f'minimum frq is {freq_min}. ')

        self.data_frame = pd.DataFrame(data={'synset': [],
                                             'writtenform': [],
                                             'freq': []})

        self.freq_min = freq_min

        for entry in tqdm(self.lexical_entry_list):

            if entry.freq >= self.freq_min:

                for sense_id in list(entry.sense_dict):
                    data_dict = {'synset':entry.sense_dict[sense_id],
                                 'writtenform':entry.written_form,
                                 'freq':entry.freq}

                    self.data_frame = \
                        self.data_frame.append(data_dict, ignore_index=True)

        return


class WprProcessor(FileSelect):
    """
    A more generic class (CropCounter) is developed. This class is kept for
    backwards compatibility.
    """

    def __init__(self, source_dict):
        import pandas as pd

        super().__init__(source_dict)

        self.word_count = []
        self.sub_word_count = []

        self.data_frame = pd.DataFrame()

        self.total_word = 0
        self.sub_total_word = 0

        self.get_file_list()

    # def get_file_list(self):
    #     self.fs_obj.get_file_list()

    @staticmethod
    def wpr_word_count(file_name):
        """

        """
        from .gen import read_file
        from os import path
        num_word = 0
        if path.isfile(file_name):
            line_list = read_file(file_name, split_flag=True)

            for line in line_list:
                if len(line) > 0:
                    if line[0] != '<':
                        num_word = num_word + 1

        else:
            processor_logger.warning(f'Input file is not found,'
                                     + f'file name: {file_name} ')

            num_word = 0

        return num_word

    def count_all_file(self):
        """

        """
        import tqdm

        n_file = len(self.file_list)
        self.word_count = [0] * n_file

        for idx in tqdm.tqdm(range(n_file)):
            file = self.file_list[idx]
            self.word_count[idx] = self.wpr_word_count(file)

        self.total_word = sum(self.word_count)

        return self.total_word

    def count_all_file_mp(self):

        import multiprocessing as mp
        import time

        t1 = time.time()
        p = mp.Pool(2)
        result = p.map(self.wpr_word_count, self.file_list)
        self.word_count = result
        t = time.time() - t1
        processor_logger.info(f'All files counted in {t:.1f} sec.')
        self.total_word = sum(self.word_count)

        self.count_sub_file_mp()

        return self.total_word

    def count_sub_file_mp(self):

        import multiprocessing as mp
        import time

        t1 = time.time()

        if len(self.word_count) == 0:
            p = mp.Pool(2)
            result = p.map(self.wpr_word_count, self.sub_file_list)
            self.sub_word_count = result
        else:
            self.sub_word_count = [0]*len(self.sub_file_list)
            for i, file in enumerate(self.sub_file_list):
                idx = self.file_list.index(file)
                self.sub_word_count[i] = self.word_count[idx]

        t = time.time() - t1
        processor_logger.info(f'All sub-files counted in {t:.1f} sec.')
        self.sub_total_word = sum(self.sub_word_count)

        return self.sub_total_word

    def convert_2_frame(self, idx):

        import pandas as pd
        from .gen import name_2_int_idx

        n_file = len(self.sub_word_count)

        year = [0] * n_file
        month = [0] * n_file
        day = [0] * n_file

        for i, file in enumerate(self.sub_file_list):
            year[i] = name_2_int_idx(file, idx, idx+3)
            month[i] = name_2_int_idx(file, idx+4, idx+5)
            day[i] = name_2_int_idx(file, idx+6, idx+7)

        data = {'year': year, 'month': month, 'day': day, 'count': self.sub_word_count}
        self.data_frame = pd.DataFrame(data=data)

        return data

    def sum_year(self, idx, year_list):

        from .gen import name_2_int_idx
        import pandas as pd

        count_list = [0] * len(year_list)

        for i, file in enumerate(self.sub_file_list):
            year = name_2_int_idx(file, idx, idx + 3)

            if year in year_list:
                idx_year = year_list.index(year)
                count_list[idx_year] = count_list[idx_year] + self.word_count[i]

        data = {'year': year_list, 'count': count_list}

        self.data_frame = pd.DataFrame(data=data)

        return sum(count_list)

    def sum_year_month(self, idx, year_list):

        pass







