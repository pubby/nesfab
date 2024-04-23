-- Copyright (c) 2023, Julius Riecke
-- This file is distributed under the Boost Software License, Version 1.0.
-- See LICENSE_1_0.txt or https://www.boost.org/LICENSE_1_0.txt 

-- Use this Lua script with 'lua_log.fab'.
-- Should work in both FCEUX (2.2.3+) and Mesen (Oct 2023 dev build)

-- Must be an unused memory address
LOG_WRITE_HOOK_ADDR = 0x00

-- Must be 8 consecutive unused ram bytes; $101..$107
LOG_ARG_STORAGE_ADDR = 0x100

-- Keep newly logged lines on screen for ~2 seconds in NTSC mode
LINE_DISAPPEAR_TICKS = 180

-- Can cause slowdown when printing lots of values
LOG_TO_CONSOLE = true

LOG_TO_SCREEN = true

LOG_X_POS = 0
LOG_Y_POS = 200
MAX_LINES_DISPLAYED = 23

local logLines = {{0,"lualog loaded"}}
local logLinesIdx = 2
local curStr = ""
local yPos
local tick = 0

if(emu.frameadvance~=nil) then
	
	--======FCEUX
	if(LOG_TO_SCREEN) then
		print("lualog loaded")
	end
	
	function writeHook()
		--callback called twice for each write?
		--https://sourceforge.net/p/fceultra/bugs/744/
		
		--fceux doesn't supply the value written (older versions at least)
		--check which instruction was used to write value
		--and read from the corresponding register
		local pc = memory.getregister("pc")
		local i3 = memory.readbyte(pc-3)
		local i2 = memory.readbyte(pc-2)
		local i1 = memory.readbyte(pc-1)
		local a = memory.getregister("a")
		local y = memory.getregister("y")
		local x = memory.getregister("x")
		
		local value = nil
		
		if     ( i3 == 0x8C ) then --[[ STY MM   ]]  value = y
		elseif ( i3 == 0x8D ) then --[[ STA MM   ]]  value = a
		elseif ( i3 == 0x8E ) then --[[ STX MM   ]]  value = x
		elseif ( i3 == 0x99 ) then --[[ STA MM,Y ]]  value = a --address = address + y
		elseif ( i3 == 0x9D ) then --[[ STA MM,X ]]  value = a --address = address + x
		else value = a --assume it's one of the indirect STA ops
		end
		
		--if(value==nil) then
		--	return
		--end
		
		if(value >= 32-31 and value < 128-31) then
			curStr = curStr .. string.char(value+31)
		else --treat other values as '\0'
			local args = {}
			for i=0,3 do
				local argAddr = LOG_ARG_STORAGE_ADDR + i*2
				args[i+1] = memory.readword(argAddr)
				--args[i] = memory.readword(LOG_ARG_STORAGE_ADDR)
			end
			curStr = string.format(curStr, args[1],args[2],args[3],args[4])
			logLines[logLinesIdx] = {tick, curStr}
			if(LOG_TO_CONSOLE) then
				print(curStr)
			end
			curStr = ""
			logLinesIdx = logLinesIdx + 1
			if(logLinesIdx>=MAX_LINES_DISPLAYED) then
				logLinesIdx = 0
			end
		end
		
	end
	
	
	function printLine(text)
		gui.text(LOG_X_POS, yPos, text)
		yPos = yPos - 9
	end
	
	memory.registerwrite(LOG_WRITE_HOOK_ADDR,writeHook)
	while(true) do
		emu.frameadvance()
		if(LOG_TO_SCREEN) then
			yPos = LOG_Y_POS
			local l = logLinesIdx-1
			for i=0,MAX_LINES_DISPLAYED-1 do
				if(l<0) then
					l = MAX_LINES_DISPLAYED-1
				end
				if(logLines[l] and tick < logLines[l][1]+LINE_DISAPPEAR_TICKS) then
					printLine(logLines[l][2])
				end
				l = l - 1
			end
		end
		tick = tick + 1
	end


else
	--======MESEN
	if(LOG_TO_SCREEN) then
		emu.log("lualog loaded")
	end
	
	function writeHook(addr, value)
		if(value >= 32-31 and value < 128-31) then
			curStr = curStr .. string.char(value+31)
		else --treat other values as '\0'
			local args = {}
			for i=0,3 do
				local argAddr = LOG_ARG_STORAGE_ADDR + i*2
				args[i+1] = emu.readWord(argAddr, emu.memType.nesDebug)
			end
			curStr = string.format(curStr, args[1],args[2],args[3],args[4])
			logLines[logLinesIdx] = {tick, curStr}
			if(LOG_TO_CONSOLE) then
				emu.log(curStr)
			end
			curStr = ""
			logLinesIdx = logLinesIdx + 1
			if(logLinesIdx>=MAX_LINES_DISPLAYED) then
				logLinesIdx = 0
			end
		end
		
	end
	
	function printLine(text)
		emu.drawString(LOG_X_POS, yPos, text, 0xFFFFFF, 0x70000000)
		yPos = yPos - 9
	end
	
	function update()
		if(LOG_TO_SCREEN) then
			yPos = LOG_Y_POS
			local l = logLinesIdx-1
			for i=0,MAX_LINES_DISPLAYED-1 do
				if(l<0) then
					l = MAX_LINES_DISPLAYED-1
				end
				if(logLines[l] and tick < logLines[l][1]+LINE_DISAPPEAR_TICKS) then
					printLine(logLines[l][2])
				end
				l = l - 1
			end
		end
		tick = tick + 1
	end

	emu.addMemoryCallback(writeHook, emu.callbackType.write, LOG_WRITE_HOOK_ADDR)
	emu.addEventCallback(update, emu.eventType.endFrame);

end
