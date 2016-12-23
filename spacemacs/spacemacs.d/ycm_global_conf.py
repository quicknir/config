import os
import ycm_core
import re
import subprocess

# These are the compilation flags that will be used in case there's no
# compilation database set (by default, one is not set).
flags = [
    '-fexceptions', '-DNDEBUG', '-std=c++14', '-x', 'c++',
    '-Wno-unused-parameter', '-I', '.', '-I', '../', '-I', '../../'
]

# Flags that get added whether or not there is a compilation database
# Always good to get warnings, and fspellchecking is necessary to get
# good fixit suggestions.
extra_flags = ['-Wall', '-Wextra', '-Wno-unused-parameter', '-fspell-checking',
               '-Wpedantic']

compilation_database_folder = ''

if os.path.exists(compilation_database_folder):
    database = ycm_core.CompilationDatabase(compilation_database_folder)
else:
    database = None

SOURCE_EXTENSIONS = ['.x.cpp', '.cpp', '.cxx', '.cc', '.c', '.m', '.mm',
                     '.t.cpp']

system_include_cache = {}


def load_system_includes(gcc_toolchain=None):
    # Typically we'll get the same system includes over and over,
    # so let's cache the solution
    if gcc_toolchain in system_include_cache:
        return system_include_cache[gcc_toolchain]

    if gcc_toolchain is None:
        gcc = []
    else:
        gcc = [gcc_toolchain]

    regex = re.compile(
        ur'(?:\#include \<...\> search starts here\:)(?P<list>.*?)(?:End of search list)',
        re.DOTALL)
    process = subprocess.Popen(
        ['clang', '-v', '-E', '-x', 'c++', '-'] + gcc,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)
    process_out, process_err = process.communicate('')
    output = process_out + process_err
    includes = []
    for p in re.search(regex, output).group('list').split('\n'):
        p = p.strip()
        if len(p) > 0 and p.find('(framework directory)') < 0:
            includes.append('-isystem')
            includes.append(p)

    system_include_cache[gcc_toolchain] = includes
    return includes


def DirectoryOfThisScript():
    return os.path.dirname(os.path.abspath(__file__))


def MakeRelativePathsInFlagsAbsolute(flags, working_directory):
    if not working_directory:
        return list(flags)
    new_flags = []
    make_next_absolute = False
    path_flags = ['-isystem', '-I', '-iquote', '--sysroot=']
    for flag in flags:
        new_flag = flag

        if make_next_absolute:
            make_next_absolute = False
            if not flag.startswith('/'):
                new_flag = os.path.join(working_directory, flag)

        for path_flag in path_flags:
            if flag == path_flag:
                make_next_absolute = True
                break

            if flag.startswith(path_flag):
                path = flag[len(path_flag):]
                new_flag = path_flag + os.path.join(working_directory, path)
                break

        if new_flag:
            new_flags.append(new_flag)
    return new_flags


def is_header_file(filename):
    extension = os.path.splitext(filename)[1]
    return extension in {'.h', '.hxx', '.hpp', '.hh'}


def GetCompilationInfoForFile(filename):
    if not is_header_file:
        return database.GetCompilationInfoForFile(filename)

    # Header files are not compilation targets, so we need to use heuristics
    # to get a reasonable set of flags

    # First attempt: find a corresponding implementation or test file
    basename = os.path.splitext(filename)[0]
    for extension in SOURCE_EXTENSIONS:
        replacement_file = basename + extension
        if os.path.exists(replacement_file):
            compilation_info = database.GetCompilationInfoForFile(
                replacement_file)
            if compilation_info.compiler_flags_:
                return compilation_info

    # Second attempt: any file in same directory
    dir = os.path.dirname(filename)

    for f in os.listdir(dir):
        if any(f.endswith(i) for i in SOURCE_EXTENSIONS):
            compilation_info = database.GetCompilationInfoForFile(
                os.path.join(dir, f))
            if compilation_info.compiler_flags_:
                return compilation_info

    return None


def FlagsForFile(filename, **kwargs):
    if database:
        # Bear in mind that compilation_info.compiler_flags_ does NOT return a
        # python list, but a "list-like" StringVec object
        compilation_info = GetCompilationInfoForFile(filename)
        if not compilation_info:
            return None

        final_flags = MakeRelativePathsInFlagsAbsolute(
            compilation_info.compiler_flags_,
            compilation_info.compiler_working_dir_) + extra_flags

        # ycmd's heuristics are broken unfortunately, and decide that compiling
        # with clang means that we are compiling c code and not c++ code,
        # leading to marking every use of try/throw/catch as an error
        # final_flags[2] = 'c++'
        final_flags[0] = "clang++"

        # To get system includes, see if gcc toolchain option specified
        final_flags = final_flags + load_system_includes(
            next((x for x in final_flags
                  if x.startswith("--gcc-toolchain")), None))

    else:
        relative_to = DirectoryOfThisScript()
        final_flags = MakeRelativePathsInFlagsAbsolute(
            flags, relative_to) + extra_flags + load_system_includes()

    return {'flags': final_flags, 'do_cache': True}
