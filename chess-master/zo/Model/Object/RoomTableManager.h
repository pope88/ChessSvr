#ifndef _RoomTableManager_H_
#define _RoomTableManager_H_
namespace Object
{
	class GameTable;
	class RoomPlayerManager;
	class RoomTableManager
	{
	public:
		RoomTableManager(UInt32 roomId, RoomPlayerManager* rooPlayerManager);
		~RoomTableManager();
		GameTable* findTable(UInt32 tableId);
		void getTables(std::vector<GameTable*> &vectable);
		GameTable* searchTable(UInt32 &nChair) {return NULL;}
		void breakAllGame();
	private:
		UInt32 _roomId;
		UInt32 maxTableNum;
		std::vector< GameTable* > _rooICoreTables; 
	};
}


#endif