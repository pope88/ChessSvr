#ifndef _TABLEMANAGER_H_
#define _TABLEMANAGER_H_
#include "GameTable.h"
#include "RoomPlayerManager.h"
#include "User.h"

namespace Object
{
	class GameRoom
	{
	public:
		GameRoom(UInt8 id);
		~GameRoom() {}
		inline UInt8 roomId() { return _roomId; }
		inline UInt32 rooICorePlayerNum()  { return _roomCorePlayers->getPlayerNum(); }
		GameTable* findTable(UInt32 tid);
		GameTable* findEmptyTable(Int8 &nChair);
		GameTable* findWaitingTable(UInt32 &nChair);
		GameTable* findCouldEnterTable(int &nChair) { return NULL;}
		bool enterUser(User *u); 
		bool outUser(User *u); 
		bool enterTable(User *u, UInt32 tableNo = 0xFFFFFFFF, UInt8 nchair = 0xFF);
		bool autoEnterTable(User *u);
		void onHeartBit() { _roomCorePlayers->onHeartBit(); }
		void breakAllGame();
	private:
		UInt8  _roomId;
		UInt32 _maxTableNum;
		std::vector<GameTable*> _arrGameTable;
		RoomPlayerManager *_roomCorePlayers;
	};
}
#endif