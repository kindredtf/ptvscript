
if(!("g_playerFOV" in getroottable()))
    {
        ::g_playerFOV <- {}
        printl("No g_playerFOV table found. Initializing.")
    }

::g_spawnTimerEnt <- null



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

::CollectEvents <- {
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
}
::ThinkFunction <- function()
{
    local spawnTimerEnt = Entities.FindByName(null, "spawn_timer")
    if (!spawnTimerEnt){
        DebugPrint("No spawn timer entity found, creating:")
        local ballSpawnerOrigin = GetBallSpawnerOrigin()
        if (ballSpawnerOrigin)
        {
            CreateSpawnTimerEntity(ballSpawnerOrigin)
            DebugPrint("Created spawn timer entity around origin:" + ballSpawnerOrigin)
        }
        else 
        {
            DebugPrint("No ball spawner found.")
        }
    }
return -1
}
::GetBallSpawnerOrigin <- function()
{
    local ballSpawner = Entities.FindByClassname(null, "info_passtime_ball_spawn")
    if (ballSpawner)
    {
        local origin = ballSpawner.GetOrigin()
        return origin
    }
    else
    {
        DebugPrint("No ball spawner found.")
    }
}

::CreateSpawnTimerEntity <- function(origin)
{
    local spawnTimerEnt = SpawnEntityFromTable("trigger_player_respawn_override", {
        origin = origin,
        spawnflags = 1,
        wait = 0.1,
        RespawnTime = 3,
        targetname = "spawn_timer",
    })

    spawnTimerEnt.SetSize(Vector(-5000, -5000, -5000), Vector(5000, 5000, 10000))
    spawnTimerEnt.SetSolid(2) // SOLID_BBOX

    if (spawnTimerEnt)
    {
        g_spawnTimerEnt = spawnTimerEnt
    }

}

::InitializeThinkFunction <- function()
{
    local worldspawn = Entities.FindByClassname(null, "worldspawn")
    AddThinkToEnt(worldspawn, "ThinkFunction")
}

InitializeThinkFunction()

EntFireByHandle(
    Entities.FindByClassname(null, "worldspawn"),
    "RunScriptCode",
    "::__CollectGameEventCallbacks(CollectEvents)",
    1, null, null);


