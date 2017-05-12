#ifndef _RoomPlayerManager_H_
#define _RoomPlayerManager_H_
#include "Packet/Builder.h"
namespace Object
{
	class User;
	class RoomPlayerManager
	{
	public:
		RoomPlayerManager() {}
		~RoomPlayerManager() {}
		bool insertPlayer(User *user);
		bool removePlayer(User *user);
		inline User* findPlayer(UInt32 pid) { _roomCorePlayers.find(pid)->second; }
		inline UInt32 getPlayerNum() { return _roomCorePlayers.size(); }
	    void broadcastRoom(Packet::Builder& builder, User *pExceptPlayer = NULL);
		void onHeartBit();
	private:
		UInt32 _roomId;
		std::unordered_map< UInt32, User* > _roomCorePlayers; 
	};
}

#endif