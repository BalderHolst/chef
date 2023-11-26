function export_signals(event)
    if event.prototype_name == "export-signals" then
        local player = game.get_player(event.player_index)
        local file_name = "chef.signals"
        local counter = 0;
        game.write_file(file_name, "", false, player.index)
        for k, v in pairs(game.item_prototypes) do
            game.write_file(file_name, "item:" .. k .. "\n", true, player.index)
            counter = counter + 1
        end
        for k, v in pairs(game.fluid_prototypes) do
            game.write_file(file_name, "fluid:" .. k .. "\n", true, player.index)
            counter = counter + 1
        end
        for k, v in pairs(game.virtual_signal_prototypes) do
            game.write_file(file_name, "virtual:" .. k .. "\n", true, player.index)
            counter = counter + 1
        end
        player.print("Exported " .. counter .. " game signals. Run `chef add signals` to use them in your code.")
    end
end

script.on_event(defines.events.on_lua_shortcut, export_signals)
