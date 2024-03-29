require("neotest").setup({
  adapters = {
    require("neotest-python")({
        -- Extra arguments for nvim-dap configuration
        dap = { justMyCode = false },
        -- Command line arguments for runner
        -- Can also be a function to return dynamic values
        args = {"--log-level", "DEBUG"},
        -- Runner to use. Will use pytest if available by default.
        -- Can be a function to return dynamic value.
        runner = "pytest",
        -- Custom python path for the runner.
        -- Can be a string or a list of strings.
        -- Can also be a function to return dynamic value.
        -- If not provided, the path will be inferred by checking for 
        -- virtual envs in the local directory and for Pipenev/Poetry configs
        python = "/home/dbohomiakov/.virtualenvs/jetwing-client/bin/python"
        -- Returns if a given file path is a test file.
        -- NB: This function is called a lot so don't perform any heavy tasks within it.
        --is_test_file = function(file_path)
        --  ...
        --end,
        
    })
  }
})
