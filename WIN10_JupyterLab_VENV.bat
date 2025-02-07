setlocal
@echo off

echo checking for python environment named my_env_WIN...
rem Check if there is a virtual environment in the current folder
if exist ".\my_venv_WIN\Scripts\activate" (
    echo "#################################################################################"
    echo  VIRTUAL ENVIRONEMENT my_env_WIN FOUND. ACTIVATING...
    call ".\my_venv_WIN\Scripts\activate"
    echo  VIRTUAL ENVIRONEMENT my_env ACTIVATED.
    echo  UPDATING requiremnts.txt...
    call uv pip install -r requirements.txt
) else (
    echo "#################################################################################"
    echo NO VIRTUAL ENVIRONEMENT my_env FOUND IN CURRENT FOLDER.
    echo  CREATING NEW ENVIRONEMENT IN SUBFOLDER my_env_WIN
    call uv venv my_venv_WIN
    echo  ACTIVATING...
    call ".\my_venv_WIN\Scripts\activate"
    echo  INSTALLING requiremnts.txt...
    call uv pip install -r requirements.txt
)

rem Note that jupyter notebook does not use the activated env
rem but jupyter lab does....
rem you should check which python executable is used in the notebook
rem this exe must be in the environments subfolder!
echo "#################################################################################"
echo   STARTING jupyterlab...
python -m jupyter lab > jupyter_log.txt 2>&1
 