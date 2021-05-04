
__author__ = "Weiwei Zhang"
__date__ = "12/30/2018"

if __name__ == '__main__':

    from ChilectoUtility.gen import FileSplit
    from ChilectoUtility.gen import rand_file_in_dir
    from ChilectoUtility.processor import SketchengineProcessor

    corpus_file_1 = '../05_sketchengine/taiwan_corpus/chinese_taiwan.vert'
    output_name_1 = '../05_sketchengine/taiwan_corpus/example_chinese_taiwan.vert'
    output_dir_1 = '../05_sketchengine/taiwan_corpus/split/'
    processed_dir_1 = '../05_sketchengine/taiwan_corpus/processed/'

    corpus_file_2 = '../05_sketchengine/zhTenTen_corpus/zhTenTen.vert'
    output_name_2 = '../05_sketchengine/zhTenTen_corpus/example_zhTenTen.vert'
    output_dir_2 = '../05_sketchengine/zhTenTen_corpus/split/'
    processed_dir_2 = '../05_sketchengine/zhTenTen_corpus/processed/'

    fs_1 = FileSplit(corpus_file_1, output_dir_1)
    fs_2 = FileSplit(corpus_file_2, output_dir_2)

    # fs_1.check_file()

    fs_1.split_head(3000000, output_name_1)
    fs_2.split_head(3000000, output_name_1)

    # using the example file for code test
    fs_1 = FileSplit(output_name_1, output_dir_1)
    fs_2 = FileSplit(output_name_1, output_dir_2)
    # ------------------------------------

    fs_1.set_split_tag('</doc>\n', 500)
    fs_2.set_split_tag('</doc>\n', 500*15)

    fs_1.split_tag()
    fs_2.split_tag()

    sp_1 = SketchengineProcessor(output_dir_1, processed_dir_1)
    sp_2 = SketchengineProcessor(output_dir_2, processed_dir_2)

    sp_1._show_bar = False

    sp_1.dir_process()
    sp_2.dir_process()

    rand_file_in_dir(output_dir_1, 33, 'rand_file_list.txt', True)

    # import pandas as pd
    #
    # df = pd.read_fwf('taiwan_corpus/line_index_2.txt', header=None)
    # df.columns = ["line"]
    #
    # line_list = df['line'].values.tolist()
    #
    # for i in range(len(line_list)):
    #     line_list[i] = line_list[i] - 173000000
    #
    # print(line_list)
    #
    # fs.get_line_byte(line_list, 'taiwan_corpus/text_wrong_encoding.txt')



