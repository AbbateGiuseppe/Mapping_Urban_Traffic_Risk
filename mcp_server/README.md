# MCP server -- query this project from Claude

This folder contains a small [MCP](https://modelcontextprotocol.io) server
that exposes `database/mapping_urban_risk.db` as a set of tools, so you can
ask Claude (Desktop, Code, or any other MCP-compatible client) questions
about this project directly -- model comparison, predicted risk by road and
scenario, methodology used -- without opening a single file.

## 1. Install dependencies

Requires Python 3.10+.

```bash
cd mcp_server
pip install -r requirements.txt
```

## 2. Build the database (if you haven't already)

```bash
cd ../database
python3 build_database.py
```

This creates `database/mapping_urban_risk.db` from the CSV exports (see
`database/README.md` for where those come from).

## 3. Try it standalone (optional)

```bash
cd mcp_server
python3 server.py
```

It will wait on stdio for an MCP client -- that's normal, it's not meant to
print anything. Press Ctrl+C to stop. To point it at a database file in a
different location, set `MAPPING_URBAN_RISK_DB`:

```bash
MAPPING_URBAN_RISK_DB=/path/to/mapping_urban_risk.db python3 server.py
```

## 4. Connect it to Claude Desktop

Edit Claude Desktop's config file (`claude_desktop_config.json` -- on
Windows: `%APPDATA%\Claude\claude_desktop_config.json`; on macOS:
`~/Library/Application Support/Claude/claude_desktop_config.json`) and add:

```json
{
  "mcpServers": {
    "mapping-urban-traffic-risk": {
      "command": "python3",
      "args": ["/absolute/path/to/Mapping_Urban_Traffic_Risk/mcp_server/server.py"]
    }
  }
}
```

Restart Claude Desktop. You should then be able to ask things like *"which
road in Centro Storico has the highest predicted accident rate on a rainy
weekend night?"* or *"which statistical techniques were used in this
project, and which gave the best-fitting model?"*.

## 5. Connect it to Claude Code / another MCP client

Most MCP clients accept the same `command` + `args` shape as above (consult
your client's docs for where that config lives). From a terminal with the
`claude` CLI, you can also register it directly:

```bash
claude mcp add mapping-urban-traffic-risk -- python3 /absolute/path/to/Mapping_Urban_Traffic_Risk/mcp_server/server.py
```

## Tools exposed

- `list_tables()` -- what's in the database and what each table means.
- `get_schema(table_name)` -- column definitions for a table.
- `run_query(sql)` -- run a read-only `SELECT` against the database directly.
- `top_risk_roads(neighborhood, month, day_period, day_type, weather, limit)`
  -- highest predicted-risk road segments for a given scenario.
- `compare_models()` -- the full GLMM model-comparison table, sorted by AIC.
- `list_techniques(category)` -- the project's methodology, by category.
