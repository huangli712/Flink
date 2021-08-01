using Documenter

makedocs(
    sitename="Flink",
    clean = false,
    authors = "Li Huang",
    format = Documenter.HTML(
        prettyurls = false,
        ansicolor = true,
    ),
    pages = [
        "Home" => "index.md",
        "Welcome" => "welcome.md",
        "Install" => "install.md",
        "Usage" => "usage.md",
        "Guide" => Any[
            "make.sys" => "guide/make.md",
        ],
    ],
)
