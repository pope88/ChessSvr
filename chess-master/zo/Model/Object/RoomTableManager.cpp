#include "Config.h"
#include "RoomTableManager.h"
#include "RoomPlayerManager.h"
#include "ZoGlobal.h"
#include "../../Game/Logic/LogicGameTable.h"
namespace Object
{
	RoomTableManager::RoomTableManager(UInt32 roomId, RoomPlayerManager* RoomPlayerManager) : _roomId(roomId)
	{
		maxTableNum = zoGlobal.tableNum();
		_rooICoreTables.resize(maxTableNum);
		for(UInt32 i = 0; i < maxTableNum; ++i)
		{
			_rooICoreTables[i] = new GameTable(UInt8(i), RoomPlayerManager);
		}
	}

	RoomTableManager::~RoomTableManager()
	{
		for(UInt32 i = 0; i < maxTableNum; ++i)
		{
			delete _rooICoreTables[i];
		}
	}

	GameTable* RoomTableManager::findTable(UInt32 tableId)
	{
		if(tableId >= maxTableNum)
			return NULL;
		return _rooICoreTables[tableId];
	}

	void RoomTableManager::getTables(std::vector<GameTable*> &vectable)
	{
		for(UInt32 i = 0; i < maxTableNum; ++i)
		{
			if(_rooICoreTables[i]->getStatus() != GameTable::TS_EMPTY)
			{
				vectable.push_back(_rooICoreTables[i]);
			}
		}
	} 

	void RoomTableManager::breakAllGame()
	{
		for(UInt32 i = 0; i < maxTableNum; ++i)
		{
			if(_rooICoreTables[i]->getCurPlayerNum() > 0)
			{
				_rooICoreTables[i]->gmBreakGame(0);
			}
		}
	}
}