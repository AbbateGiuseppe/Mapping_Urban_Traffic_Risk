# MCP server -- query this project from Claude

This folder contains a small [MCP](https://modelcontextprotocol.io) server
that exposes `database/mapping_urban_risk.db` as a set of tools, so you can
ask Claude (Desktop, Code, or any other MCP-compatible client) questions
about this project directly -- model comparison, predicted risk by road and
scenario, methodology used -- without opening a single file.

A copy of the database (`mapping_urban_risk.db`) is bundled right in this
folder, alongside `database/mapping_urban_risk.db` at the repo root. This is
so the server works unmodified no matter which of the two connection
methods below you use -- see "Which connection method should I use?".

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

Both `database/mapping_urban_risk.db` (repo root) and
`mcp_server/mapping_urban_risk.db` (this folder) are already built and
committed to the repo, so this step is normally **not needed**. Only
rebuild if you've regenerated the CSV exports yourself (see
`database/README.md`) -- and if you do, remember to copy the freshly built
file into *both* locations (see that README for the exact command).

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

There are two ways to do this, depending on your Claude Desktop version.
Not sure which you have? Try 4a first; if "Edit config" doesn't show a
`claude_desktop_config.json` with an `mcpServers` key you can edit, your
app manages local MCP servers as **extensions** instead -- use 4b.

### 4a. Classic method: edit `claude_desktop_config.json`

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
Windows -- check the system tray and quit it from there if needed).

### 4b. Newer Claude Desktop builds: install as a local extension

Some Claude Desktop builds (the ones with Cowork integrated) don't read
`claude_desktop_config.json` for local MCP servers at all -- they manage
them as **Desktop Extensions** instead, installed from a folder containing
a `manifest.json`. This repo already ships one at `mcp_server/manifest.json`,
so:

1. Open **Settings -> Extensions** in Claude Desktop.
2. Click **"Install unpacked extension"**.
3. Select the `mcp_server` folder (the one containing `manifest.json`,
   `server.py` and `mapping_urban_risk.db`) -- not the `manifest.json` file
   itself.
4. It should install and connect automatically -- Claude Desktop detects a
   Python interpreter on your system on its own (check
   Settings -> Extensions -> "Developer tools" to see which one it found).

No path editing needed: this install method **copies** the `mcp_server`
folder into Claude's own extensions directory, which is exactly why a copy
of the database is bundled inside this folder in the first place -- a
server relying only on `../database/mapping_urban_risk.db` (one level up)
would lose access to it once copied.

If you ever need to update the extension after pulling repo changes,
remove it (Settings -> Extensions -> mapping-urban-traffic-risk ->
Uninstall) and install it again the same way -- it doesn't auto-update
from a re-copied folder.

Once connected (either method), you should be able to ask things like
*"which road in Centro Storico has the highest predicted accident rate on a
rainy weekend night?"* or *"which statistical techniques were used in this
project, and which gave the best-fitting model?"*.

## 5. Connect it to Claude Code / another MCP client

Most MCP clients accept the same `command` + `args` shape as in 4a above
(consult your client's docs for where that config lives). From a terminal
with the `claude` CLI, you can also register it directly:

```bash
claude mcp add mapping-urban-traffic-risk -- python /absolute/path/to/Mapping_Urban_Traffic_Risk/mcp_server/server.py
```

## Troubleshooting

- **Claude doesn't show the tools / the server doesn't appear**: double
  check the JSON is valid (no trailing commas, matching braces), that the
  path uses double backslashes on Windows (`\\`) or forward slashes, and
  that you fully restarted the app.
- **`ModuleNotFoundError: No module named 'idna'`** (or a similar missing
  submodule of `mcp`'s own dependencies): re-run
  `pip install -r requirements.txt` -- `idna` is pinned explicitly in that
  file for exactly this reason, but if your install predates that pin,
  `pip install idna` fixes it directly. Make sure you install into the
  *same* Python that your MCP client actually runs (if in doubt, use the
  full path to that interpreter instead of a bare `python`/`python3`).
- **`ModuleNotFoundError: No module named 'mcp'`**: the `pip install` in
  step 1 ran against a different Python than the one in your config's
  `"command"`. Use the same `python`/`python3`/full path consistently in
  both places.
- **"Database not found" / `FileNotFoundError`**: `server.py` looks for the
  database in two places, in order: a copy named `mapping_urban_risk.db`
  sitting right next to `server.py` (this is what the extension-install
  method in 4b relies on, and it's committed to the repo already), and
  `../database/mapping_urban_risk.db` (the repo-root layout, used when
  running the script in place as in 4a/5). If you've restructured the repo
  and neither exists, set `MAPPING_URBAN_RISK_DB` explicitly to the file's
  absolute path (see step 3).

## Tools exposed

- `list_tables()` -- what's in the database and what each table means.
- `get_schema(table_name)` -- column definitions for a table.
- `run_query(sql)` -- run a read-only `SELECT` against the database directly.
- `top_risk_roads(neighborhood, month, day_period, day_type, weather, limit)`
  -- highest predicted-risk road segments for a given scenario.
- `compare_models()` -- the full GLMM model-comparison table, sorted by AIC.
- `list_techniques(category)` -- the project's methodology, by category.
