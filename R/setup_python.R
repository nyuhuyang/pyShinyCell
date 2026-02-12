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
#' 1. Clears existing Python environment variables to avoid interference
#' 2. Creates a Python virtual environment if it doesn't exist
#' 3. Upgrades pip, setuptools, and wheel
#' 4. Installs required Python packages
#' 5. Activates the environment in reticulate
#' 6. Verifies all packages can be imported
#'
#' The function is idempotent: running it multiple times on the same environment
#' is safe and will skip re-creation if the environment already exists.
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
    reticulate::virtualenv_create(envname = venv_name, python = python_exe)
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
