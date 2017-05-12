#ifndef _TABLEMANAGER_H_
#define _TABLEMANAGER_H_
#include "GameTable.h"
#include "UserManager.h"

namespace Object
{
	class CTableManager
	{
	private:

	public:
		CTableManager(int nRoomID, UserManager* pPlayerManager);
		~CTableManager(void);
		GameTable* FindTable(int tableID);
		GameTable* SeacheTable(int& nChair, const std::string& strIP);
		void BreakAllGame();
		GameTable* FindEmptyTable(int& nChair);
		GameTable* FindWaitingTable(int& nChair);
	private:
		int	m_nRoomID;
		//CGameTable*	m_arrGameTable[MAX_TABLE_NUM];
		int m_nMaxTableNum;
		std::vector<GameTable*> m_arrGameTable;
	};
}


//class TableManager
//{
//public:
//	TableManager(int roomid, RoomPlayerManager* pPlayerManager) {}
//	~TableManager() {}
//	GameTable* findTablById(int tableId) {}
//	GameTable* findEmptyTable(int &nChair){}
//	GameTable* findWaitingTable(int &nChair){}
//	GameTable* findCouldEnterTable(int &nChair) {}
//public
//};
#endif