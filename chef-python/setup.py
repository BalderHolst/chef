from setuptools import setup, find_packages

setup(
    name = "chef",
    version = '0.0.1',
    author = "Balder W. Holst",
    author_email = "",
    packages = find_packages(where="src"),
    description = "Communication with DTMF tones",
    long_description_content_type = "text/markdown",
    package_dir={'': 'src'},
    install_requires = [ "pytest" ],
    python_requires = ">=3.8",
    license = "MIT",
)
