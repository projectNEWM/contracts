import re


def update_rust_file(filename):
    with open(filename, 'r') as f:
        content = f.read()

    # Match occurrences of the entire function including list.and([...])
    pattern = r'list\.and\(\[(.*?)\]\)'
    updated_content = re.sub(pattern, r'and {\1}', content)



    print(updated_content)

    # Remove square brackets within the and statement
    # updated_content = re.sub(r'and\s*\{([\s\S]*?)\}', r'and {\1', updated_content, flags=re.DOTALL)


    # with open(filename, 'w') as f:
    #     f.write(updated_content)

if __name__ == "__main__":
    rust_filename = "/home/logic/Documents/Work/ProjectNewm/contracts/aiken-contracts/fractional/lib/tests/prices.ak"
    update_rust_file(rust_filename)
    print(f"{rust_filename} has been updated.")
