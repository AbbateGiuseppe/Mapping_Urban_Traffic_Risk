# MCP server -- query this project from Claude

This folder contains a small [MCP](https://modelcontextprotocol.io) server
that exposes `database/mapping_urban_risk.db` as a set of tools, so you can
ask Claude (Desktop, Code, or any other MCP-compatible client) questions
about this project directly -- model comparison, predicted risk by road and
scenario, methodology used -- without opening a single file.

The steps below work for anyone who clones this repo, on Windows, macOS or
Linux -- not just the original author.

## 0. Clone the repo

```bash
git clone https://github.com/AbbateGiuseppe/Mapping_Urban_Traffic_Risk.git
cd Mapping_Urban_Traffic_Risk
```

## 1. Install dependencies

Requires **Python 3.10+**. Check what's available:

```bash
python --version     # Windows (also try "py --version" if this fails)
python3 --version    # macOS / Linux
```

Then, from the repo root:

```bash
cd mcp_server
pip install -r requirements.txt        # or: pip3 install -r requirements.txt
```

(Optional but recommended: do this inside a virtual environment --
`python -m venv .venv` then activate it -- so the dependency doesn't land in
your global Python install.)

## 2. Database

`database/mapping_urban_risk.db` is already built and committed to the
repo, so this step is normally **not needed**. Only rebuild it if you've
regenerated the CSV exports yourself (see `database/README.md`):

```bash
cd ../database
python build_database.py        # Windows
python3 build_database.py       # macOS / Linux
```

## 3. Try it standalone (optional sanity check)

```bash
cd mcp_server
python server.py       # Windows
python3 server.py      # macOS / Linux
```

It will sit there waiting on stdio for an MCP client -- that's normal, it's
not meant to print anything. Press Ctrl+C to stop. If it exits immediately
with an error instead, read the message (usually a missing dependency or
the database not being found) before moving on to step 4. To point it at a
database file in a different location, set `MAPPING_URBAN_RISK_DB`:

```bash
# Windows (PowerShell)
$env:MAPPING_URBAN_RISK_DB = "C:\path\to\mapping_urban_risk.db"; python server.py

# macOS / Linux
MAPPING_URBAN_RISK_DB=/path/to/mapping_urban_risk.db python3 server.py
```

## 4. Connect it to Claude Desktop

First, get the **full absolute path** to `server.py` on your machine (you'll
need this exact path in the config below):

```bash
# Windows (PowerShell), from inside mcp_server/
(Get-Item .\server.py).FullName

# macOS / Linux, from inside mcp_server/
pwd  # then append /server.py
```

Then edit Claude Desktop's config file and add an entry under `mcpServers`.
The config file lives at:

- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`
  (paste that path into File Explorer's address bar, or open it directly:
  `notepad $env:APPDATA\Claude\claude_desktop_config.json` in PowerShell)
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Linux**: `~/.config/Claude/claude_desktop_config.json`

If the file doesn't exist yet, create it with just this content (adjusting
the path and the `python`/`python3` command to what worked in step 1):

```json
{
  "mcpServers": {
    "mapping-urban-traffic-risk": {
      "command": "python",
      "args": ["C:\\Users\\yourname\\path\\to\\Mapping_Urban_Traffic_Risk\\mcp_server\\server.py"]
    }
  }
}
```

macOS/Linux example:

```json
{
  "mcpServers": {
    "mapping-urban-traffic-risk": {
      "command": "python3",
      "args": ["/Users/yourname/path/to/Mapping_Urban_Traffic_Risk/mcp_server/server.py"]
    }
  }
}
```

If the file already has other entries under `mcpServers`, just add
`"mapping-urban-traffic-risk": { ... }` as one more key alongside them
(don't replace the whole file).

**Fully quit and reopen Claude Desktop** (closing the window isn't enough on
Windows -- check the system tray and quit it from there if needed). You
should then be able to ask things like *"which road in Centro Storico has
the highest predicted accident rate on a rainy weekend night?"* or *"which
statistical techniques were used in this project, and which gave the
best-fitting model?"*.

## 5. Connect it to Claude Code / another MCP client

Most MCP clients accept the same `command` + `args` shape as above (consult
your client's docs for where that config lives). From a terminal with the
`claude` CLI, you can also register it directly:

```bash
claude mcp add mapping-urban-traffic-risk -- python /absolute/path/to/Mapping_Urban_Traffic_Risk/mcp_server/server.py
```

## Troubleshooting

- **Claude doesn't show the tools / the server doesn't appear**: double
  check the JSON is valid (no trailing commas, matching braces), that the
  path uses double backslashes on Windows (`\\`) or forward slashes, and
  that you fully restarted the app.
- **`ModuleNotFoundError: No module named 'mcp'`**: the `pip install` in
  step 1 ran against a different Python than the one in your config's
  `"command"`. Use the same `python`/`python3`/full path consistently in
  both places.
- **Database not found**: `database/mapping_urban_risk.db` must exist
  relative to `mcp_server/server.py` (one level up, in `../database/`), or
  you must set `MAPPING_URBAN_RISK_DB` explicitly.

## Tools exposed

- `list_tables()` -- what's in the database and what each table means.
- `get_schema(table_name)` -- column definitions for a table.
- `run_query(sql)` -- run a read-only `SELECT` against the database directly.
- `top_risk_roads(neighborhood, month, day_period, day_type, weather, limit)`
  -- highest predicted-risk road segments for a given scenario.
- `compare_models()` -- the full GLMM model-comparison table, sorted by AIC.
- `list_techniques(category)` -- the project's methodology, by category.
