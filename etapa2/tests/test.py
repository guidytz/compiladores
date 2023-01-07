import os
import subprocess
import sys

from colorama import Fore, Style


def success(string: str) -> str:
    return f"{Fore.GREEN}{string}{Style.RESET_ALL}"


def fail(string: str) -> str:
    return f"{Fore.RED}{string}{Style.RESET_ALL}"


def test_filter(names: list[str], filter_str: str | None) -> list:
    names.sort()
    if filter_str is None:
        return names
    return list(filter(lambda x: x.find(filter_str) != -1, names))


def main(args):
    filter_str = args[1] if len(args) == 2 else None
    inputs = test_filter(os.listdir(path="inputs"), filter_str)
    outpus = test_filter(os.listdir(path="outputs"), filter_str)
    assert inputs == outpus

    passed_tests = 0
    failed_tests = 0

    for input_file, output_file, i in zip(inputs, outpus, range(1, len(inputs) + 1)):
        print(f"Test {i} ({Fore.CYAN}{input_file}{Style.RESET_ALL})")
        with open(f"inputs/{input_file}") as file:
            print(f"Input: {file.read()}")

        proc = subprocess.run([f"../etapa2 < inputs/{input_file}"], shell=True, capture_output=True, text=True)
        return_code = proc.returncode
        output = proc.stdout

        with open(f"outputs/{output_file}") as file:
            expected = file.read()

            if (len(expected) == 0 and return_code == 0 and len(output) == 0) or (output == expected):
                passed_tests += 1
                print(success("SUCCESS!\n"))
                continue

            failed_tests += 1
            print(fail("FAIL!"))
            print(f"Expected output: {expected}")
            print(f"Got: {output[:-1]}")
            print(f"With error code: {return_code}\n")

    print(f"{success('Passed:')} {passed_tests} tests")
    print(f"{fail('Failed:')} {failed_tests} tests")


if __name__ == "__main__":
    main(sys.argv)
