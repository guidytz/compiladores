import os
import subprocess


def main():
    inputs = os.listdir(path="inputs")
    outpus = os.listdir(path="outputs")
    assert inputs == outpus

    passed_tests = 0
    failed_tests = 0

    for input_file, output_file, i in zip(inputs, outpus, range(len(inputs))):
        print(f"Test {i}")
        with open(f"inputs/{input_file}") as file:
            print(f"Input: {file.read()}")

        outs = subprocess.Popen([f"../etapa1 < inputs/{input_file}"], shell=True, stdout=subprocess.PIPE)
        (output, _) = outs.communicate()
        output_entry = output.decode("utf-8")
        print(f"Output: {output_entry[:-1]}")

        with open(f"outputs/{output_file}") as file:
            expected = file.read()

            if output_entry == expected:
                passed_tests += 1
                print("SUCCESS!\n")
                continue

            failed_tests += 1
            print("FAIL!")
            print(f"Expected output: {expected}\n")

    print(f"Passed: {passed_tests} tests")
    print(f"Failed: {failed_tests} tests")


if __name__ == "__main__":
    main()
