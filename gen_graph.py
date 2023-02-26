import sys


def main(args):
    file_name = args[1]
    with open(f"{file_name}") as file:
        lines = file.readlines()
        lines = [line.replace("0x", "x") for line in lines]
        labels = [line[:-1] for line in lines if line.find("label") != -1]
        nodes = [line[:-1].replace(",", " ->") + ";" for line in lines if line.find("label") == -1]

        print("digraph G {")
        for label in labels:
            print(f"    {label}")
        for node in nodes:
            print(f"    {node}")
        print("}")


if __name__ == "__main__":
    main(sys.argv)
