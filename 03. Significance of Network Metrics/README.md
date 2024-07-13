# Significance of Network Metrics

## Execution of Software
- Generate a virtual environment of Python, with anaconda:
    ```
        conda create --name csn-lab3 python=3.9.9
    ```
- Activate virtual environment:
    ```
        conda activate csn-lab3
    ```
- Install the dependencies used during the development:
    ```
        pip install -r requirements.txt
    ```
- Include under the path `./data/raw` all the `${language}_syntactic_dependency_network.txt` files.
- Execute the scripts with the respective order:
    - `preprocessing.py`
    - `hypotheses_testing.py`