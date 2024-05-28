## Development

```bash
# Create a Python virtual environment
python3 -m venv venv

# Activate the virtual environment
source venv/bin/activate

# Install required Python packages
pip install -r requirements.txt
```

Below are some examples of accessing the data

# Sources

[source]
type = "N2C"
address = ["Unix", "/path/to/node.socket"]
magic = "preprod"


# Intersects

[source.intersect]
type = "Point"
value = [34117450, "529c536c02fa056874ff2e9aaa56d79b7e4d3b0fceafc1321cbd048dd9361808"]

[source.intersect]
type = "Origin"
