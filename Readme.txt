myWormNET 2.0
by The_CyberShadow, modifications (1.0 -> 2.0) by Expro



1. License:

This software is FREEWARE and is distributed "AS IS" - this software is 
absolutely free, and the authors are not responsible for any damages caused by 
the use or misuse of this software.

Please don't steal our work. You may use whatever part of this program for
whatever non-commercial purposes, but give us credits (mention our names somewhere
 in the documentation or about dialog).


 
2. Changes 1.0 -> 2.0

	- clean up code, rewrote it to the object oriented (only WormNAT module is still
	procedural)
	- multiple channels
	- channels supports schemes, verison checking, topics and black/white list
	- global banlist
	- minor anti-flood system
	- few commands for ops
	- one 'waiting' game per IP

	Changes 2.0 -> 2.0.0.1
	- clean up my own code
	- added few more commands
	- fix few log lines
	
	Changes 2.0.0.1 -> 2.0.0.2
	- fixed bug with ProSnooper
	- fixed bug with games list
	- added Message of the Day
	
	Changes 2.0.0.2 -> 2.0.0.3
	- fixed newly created bug with games
 

3. Config:

a) ServerSettings.ini:
	
	[Server]
	Address - server's address
	
	HTTPPort - port for all HTTP request, 99% chance it is 80
	
	IRCPort - port for IRC chat, W:A always uses 6667
	
	NATPort - port for WormNAT, default 17018
	
	MaxConnections - set how many users can be connected to the server
	
	RefreshInterval - milliseconds between refreshing list of channels and users
	on the server (highter - less network trafic, but also less actual info),
	set 0 for auto-interval mode
	
	OperatorPassword - password for global ops mode
	
	[Log]
	Mask - filter for console log, see list of masks below
	
	FileMask - filer for 'Log.txt', see list of masks below
	
	ShowTags - 1 will add masks to the console log, 0 will remove it
	
	WriteTags - 1 will add masks to the file log (Log.txt), 0 will remove it
	
	
	Masks List:
	ERROR - displays all error messages
	USER - displays all users events (log in, chat talks, joining/leaving channels etc)
	CHANNEL - displays all channels enevnts
	GENERAL - decoration only, displays 3 messages (serves is loading, server is up, server is down)
	it is good idea to have it
	NETWORK - shows all HTTP and IRC informations
	IN - display all input trafic, both files and network
	OUT - display all input trafic, both files and network, recommended: debug only
	WSA - shows all info about WinSock events
	SERVER - all events in TServer class
	FLOOD - messages from anti-flood system
	BAN - messages from access system
	
	Use | as separator for masks, example:
	
	Mask: USER|CHANNEL|WSA - will show all users and channels events + messages
	about WinSock
	
	
	b) Channels:
	
	Adding channel: create *.txt file in 'Channels' folder (all names allowed, but
	files order -> channels order), and fill it with this commans
	(one per line, any sequence). Commands:
	
	N <name> - name of channel, spaces allowed
	T <topic> - topic of channel, spaces also allowed
	V <version> - Worms version restriction, set 0 for all versions
	W <0/any> - 0 for black list mode, anything else for white list mode
	S <scheme> - info from WKB: http://worms2d.info/WormNET_(Worms_Armageddon)
	B <name or ip> - record for black/white list
	
	Check included channels if You need.
	
	Black List - users/ips on list cannot stay/join on channel
	White List - ony users/ips on list can stay/join channel
	
	c) Banlist
	
	Add lines to the 'Banlist.txt', one IP/Nick per line.
	
	
	
	4. Custom Commands:
	
	Every command begins with $. List:
	! Names are case sensitive !
	
	
	User commands:
	
	oper <password> - logging to the operator mode
	help - list of commands
	admin <reason> - request for admin/operator intervention
	
	
	Operator commands:
	
	ban <name or ip> - add <...> to the global banlist
	banlist - shows all names/ips on global banlist
	banremove <name or ip> - removes <...> from global banlist 
	black <name or ip> - add <...> to the black/white list
	blacklist - shows all names/ips on black/white list for current channel
	blackremove <name or ip> - remove <...> from black/white list
	switch - changes channel mode from black -> white or from white -> black
	info <name> - shows nick, ip, game version and localization of <...>
	flood <name> - how much flood points have user, more then 10 000 - mute for 30 sec,
	more then 50 000 - auto-kick, 750 + length of message * 10 per line on IRC
	listmode - is current channel on white or black mode
	operlist - list of current operators on server
	userlist - list of users on server
	
	Example:
	$oper abc - logged as operator
	$ban Expro - ups, i have banned myself
	$flood Expro - heh, 9999 flood points, almost muted
	$switch - current black list switched to the white list, everyone's gone, im alone...
	
	
	5. Anti-flood:
	
	- spamming on chat will result in mute, continual spamming will result in auto-kick
	- only one 'waiting' game per IP, if new one appears, old one is deleted
	
	
	6. Message of the Day
	
	Put Your MOTG in the MOTD.txt file.