local button = table.deepcopy(data.raw.shortcut["import-string"])

button.name = "export-signals"
button.action = "lua"
button.stype = "red"
button.localised_name = "Export Signal"
button.localised_description = "Export all game signals to be used in chef"
data:extend{button}
