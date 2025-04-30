
::EnumerateTable <- function(table) {
    printl("{");
    foreach (key, value in table) {
        printl("    " + key + ": " + value);
    }
    printl("}")
}


    if(!("g_playerFOV" in getroottable()))
    {
        ::g_playerFOV <- {}
        printl("No g_playerFOV table found. Initializing.")
    }




::CollectEventsInScope <- function(events)
{
    local events_id = UniqueString()
    getroottable()[events_id] <- events
    local events_table = getroottable()[events_id]
    foreach (name, callback in events) events_table[name] = callback.bindenv(this)
    local cleanup_user_func, cleanup_event = "OnGameEvent_scorestats_accumulated_update"
    if (cleanup_event in events) cleanup_user_func = events[cleanup_event].bindenv(this)
    events_table[cleanup_event] <- function(params)
    {
        if (cleanup_user_func) cleanup_user_func(params)
        delete getroottable()[events_id]
    }
    __CollectGameEventCallbacks(events_table)
}

CollectEventsInScope({
    OnGameEvent_teamplay_round_start = function(params)
    {
        local player = null
        local steamID = null
        while (player = Entities.FindByClassname(player, "player"))
        {
            steamID = GetSteamID(player)
            if (steamID in g_playerFOV)
            {
                SetPlayerFOV(player, g_playerFOV[steamID])
                DebugPrint("Restored FOV " + g_playerFOV[steamID] + " for player " + player)
            }
        }

    }

    OnGameEvent_player_spawn = function(params)
    {
        if (!("userid" in params))
        return;
        
        try 
        {
            local player = GetPlayerFromUserID(params.userid)
            local steamID = null
            if (!player || !player.IsValid())
            {
                return
            }
            steamID = GetSteamID(player)
            if (steamID in g_playerFOV)
            {
                SetPlayerFOV(player, g_playerFOV[steamID])
                DebugPrint("Restored FOV " + g_playerFOV[steamID] + " for player " + player)
            }
        } 
        catch(e) 
        {
            DebugPrint("Error in player_spawn event: " + e);
        }
    }
    OnGameEvent_player_say = function(EventData)
    {
        local words = split(EventData.text, " ", true);
        local strTextFirstWord = words[0];

        if (strTextFirstWord.len() > 1 && strTextFirstWord[0] == '!')
        {
            local command = strTextFirstWord.slice(1).tolower()
            local hPlayer = GetPlayerFromUserID(EventData.userid)
            if (!hPlayer || !hPlayer.IsValid())
                return
            local steamID = GetSteamID(hPlayer)
            DebugPrint("Player " + hPlayer + " with SteamID " + steamID + " sent command: " + command)
            switch (command)
            {
                case "fov":
                if (words.len() > 1)
                {
                    try 
                    {
                        local fovValue = words[1].tointeger()
                        if (fovValue > 110)
                        {
                        fovValue = 110
                        }
                        else if (fovValue < 90)
                        {
                        fovValue = 90
                        }
                        g_playerFOV[steamID] <- fovValue; // Store the FOV setting
                        SetPlayerFOV(hPlayer, fovValue)
                        DebugPrint("Set FOV to " + fovValue + " for player " + hPlayer)                    
                    } catch (e) {
                            DebugPrint("Invalid FOV value")
                    }
                }
                break;
            }
        }
    }

})

::DebugPrint <- function(msg) 
{
    printl("[VScript Debug] " + msg);
}

::GetSteamID <- function(player) 
{
    local steamID = NetProps.GetPropString(player, "m_szNetworkIDString")
    if (steamID != null)
    {
        return steamID
    }
}

::SetPlayerFOV <- function(player, fov)
{
    if (!player || !player.IsValid())
        return;
    
    NetProps.SetPropInt(player, "m_iFOV", fov);
}

