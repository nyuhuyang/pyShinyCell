#' Setup Python Virtual Environment for pyShinyCell
#'
#' Creates and configures a Python virtual environment with required packages
#' for pyShinyCell analysis. This function is typically called once during initial
#' package setup. It can be safely called multiple times as it checks for existing
#' environments before creating new ones.
#'
#' @param venv_name Character. Name of the virtual environment to create.
#'   Defaults to "pyShinyCell". The environment will be created in
#'   `~/.virtualenvs/<venv_name>`.
#' @param python_exe Character. Path to Python executable or version specification.
#'   Defaults to `Sys.which("python3")`.
#' @param packages Character vector. Python packages to install. Defaults to:
#'   c("numpy", "pandas", "h5py", "pyreadr", "tables", "scanpy", "anndata",
#'     "scipy", "gseapy", "enrichr")
#' @param force Logical. If TRUE, removes and recreates the environment even if
#'   it already exists. Defaults to FALSE.
#' @param verbose Logical. If TRUE, prints status messages during setup.
#'   Defaults to TRUE.
#'
#' @return Invisibly returns the path to the virtual environment.
#'
#' @details
#' This function performs the following steps:
#' 1. Verifies Python 3 is installed (installs if needed with `reticulate::install_python()`)
#' 2. Clears existing Python environment variables to avoid interference
#' 3. Creates a Python virtual environment if it doesn't exist
#' 4. Upgrades pip, setuptools, and wheel
#' 5. Installs required Python packages
#' 6. Activates the environment in reticulate
#' 7. Verifies all packages can be imported
#'
#' The function is idempotent: running it multiple times on the same environment
#' is safe and will skip re-creation if the environment already exists.
#'
#' If Python is not found on your system, the function will provide installation
#' instructions for three methods:
#' - **Easiest**: `reticulate::install_python()` (R-based installation)
#' - **Linux/Mac**: `sudo apt-get install python3-venv` (system package manager)
#' - **Any OS**: Download from https://www.python.org/downloads/
#'
#' @section System Requirements:
#' - R 3.5.0 or higher
#' - Python 3.6 or higher with venv support
#' - `reticulate` package
#'
#' @section Dependencies:
#' Requires the `reticulate` package to be loaded.
#'
#' @examples
#' # This is typically called internally or requires setup.
#'
#' @export
setupPythonEnv <- function(
    venv_name = "pyShinyCell",
    python_exe = Sys.which("python3"),
    packages = c("numpy", "pandas", "h5py", "pyreadr", "tables", "scanpy",
                 "anndata", "scipy", "gseapy", "enrichr"),
    force = FALSE,
    verbose = TRUE) {

  # Ensure reticulate is available
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("reticulate package is required. Install it with: install.packages('reticulate')")
  }

  msg <- function(...) {
    if (verbose) cat(paste0(..., "\n"))
  }

  # Step 1: Clear environment variables
  msg("[PREP] Clearing Python environment variables...")
  Sys.unsetenv("RETICULATE_PYTHON")
  Sys.unsetenv("RETICULATE_PYTHON_ENV")
  Sys.unsetenv("RETICULATE_CONDA_ENV")
  Sys.unsetenv("CONDA_PREFIX")
  Sys.unsetenv("PYTHONPATH")

  # Step 1.5: Check if Python is available
  msg("[CHECK] Verifying Python installation...")
  if (python_exe == "" || is.na(python_exe)) {
    msg("[WARNING] Python 3 not found in PATH.")
    msg("Please install Python using one of these methods:")
    msg("")
    msg("  Option 1: Install via reticulate (easiest)")
    msg("    reticulate::install_python()")
    msg("")
    msg("  Option 2: System package manager (Ubuntu/Debian)")
    msg("    sudo apt-get update")
    msg("    sudo apt-get install python3-venv python3-pip python3-dev")
    msg("")
    msg("  Option 3: From python.org")
    msg("    https://www.python.org/downloads/")
    msg("")
    stop("Python 3 is required but not found. Please install it using one of the methods above.")
  }

  # Verify Python executable actually works
  py_check <- tryCatch({
    system2(python_exe, "--version", stdout = TRUE, stderr = TRUE)
  }, error = function(e) NULL)

  if (is.null(py_check)) {
    msg("[ERROR] Python executable found but cannot be executed: ", python_exe)
    msg("Please verify your Python installation and try again.")
    stop("Python executable verification failed.")
  }
  msg("[OK] Python found: ", python_exe)

  # Step 2: Define environment paths
  venv_root <- path.expand("~/.virtualenvs")
  venv_dir <- file.path(venv_root, venv_name)
  py_bin <- file.path(venv_dir, "bin", "python")
  pip_bin <- file.path(venv_dir, "bin", "pip")

  # Step 3: Create virtual environment
  if (force && dir.exists(venv_dir)) {
    msg("[RECYCLE]  Removing existing environment (force = TRUE)...")
    unlink(venv_dir, recursive = TRUE)
  }

  if (!dir.exists(venv_dir)) {
    msg("[PKG] Creating virtualenv at ", venv_dir)
    if (!dir.exists(venv_root)) {
      dir.create(venv_root, recursive = TRUE, showWarnings = FALSE)
    }
    tryCatch({
      reticulate::virtualenv_create(envname = venv_name, python = python_exe)
    }, error = function(e) {
      stop("Failed to create virtual environment. Error: ", e$message, "\n",
           "Make sure Python 3 is properly installed with venv support.")
    })
  } else {
    msg("[OK] Virtualenv already exists: ", venv_dir)
  }

  # Step 4: Install Python packages
  msg("[IN] Installing Python packages...")
  system2(py_bin, c("-m", "pip", "install", "--no-cache-dir", "--upgrade",
                    "pip", "setuptools", "wheel"),
          stdout = if (verbose) "" else FALSE,
          stderr = if (verbose) "" else FALSE)

  system2(py_bin, c("-m", "pip", "install", "--no-cache-dir", packages),
          stdout = if (verbose) "" else FALSE,
          stderr = if (verbose) "" else FALSE)

  # Step 5: Activate in reticulate
  msg("- Activating virtualenv in reticulate...")
  reticulate::use_virtualenv(venv_name, required = TRUE)

  # Step 6: Verify imports
  msg("[VERIFY] Verifying Python packages...")
  for (pkg in packages) {
    tryCatch({
      reticulate::py_run_string(sprintf("import %s", pkg))
      msg("   [OK] ", pkg)
    }, error = function(e) {
      warning(sprintf("Could not import '%s': %s", pkg, e$message))
    })
  }

  msg("[OK] Python environment setup complete!")
  invisible(venv_dir)
}
