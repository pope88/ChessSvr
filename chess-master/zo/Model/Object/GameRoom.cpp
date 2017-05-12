#include "Config.h"
#include "GameRoom.h"
#include "ZoGlobal.h"
#include "GameTable.h"

#include <cassert>
namespace Object
{
	GameRoom::GameRoom(UInt8 id):_roomId(0), _maxTableNum(zoGlobal.tableNum())
	{
		_arrGameTable.resize(_maxTableNum);
		_roomCorePlayers = new(std::nothrow)RoomPlayerManager();
		assert(_roomCorePlayers != NULL);
		
		/*****for(size_t i = 0; i < _maxTableNum; ++i)
		{
			_arrGameTable[i] = new GameTable(i, _roomCorePlayers);
		}************/
	}

	void GameRoom::breakAllGame()
	{
		for(auto it = _arrGameTable.begin(); it != _arrGameTable.end(); ++it)
		{
			/*********(*it)->gmBreakGame(0);********/
		}
	}

	GameTable* GameRoom::findTable(UInt32 tid)
	{
		for (auto it = _arrGameTable.begin(); it != _arrGameTable.end(); ++it)
		{
			/*******if (tid == (*it)->getTableID())
			{
				return *it;
			}********/
		}
		return NULL;
	}

	GameTable* GameRoom::findEmptyTable(Int8 &nChair)
	{
		for (size_t i = 0; i < _arrGameTable.size(); ++i)
		{
			/********if(((GameTable*)_arrGameTable[i])->getStatus() == GameTable::TS_EMPTY)
			{
				nChair = _arrGameTable[i]->findEmptyChair();
				return _arrGameTable[i];
			}*********/
		}
		return NULL;
	}

	GameTable* GameRoom::findWaitingTable(UInt32 &nChair)
	{
		for (size_t i = 0; i < _arrGameTable.size(); ++i)
		{
			/********if(((GameTable*)_arrGameTable[i])->getStatus() == GameTable::TS_WATING)
			{
				nChair = _arrGameTable[i]->findEmptyChair();
				return _arrGameTable[i];
			}********/
		}
		return NULL;
	}

	bool GameRoom::enterTable(User *u, UInt32 tableNo, UInt8 nchair)
	{
		if (u == NULL)
		{
			return false;
		}
		GameTable *pt = NULL;
		if (tableNo < _arrGameTable.size())
		{
			 pt = _arrGameTable[tableNo];
		}
		if (pt == NULL)
		{
			return false;
		}
		/****if (pt->onUserEnter(u, nchair))
		{
			pt->canStartGame();
		}***********/
		
		return true;
	}

	bool GameRoom::autoEnterTable(User *u)
	{
		for(auto it = _arrGameTable.begin(); it != _arrGameTable.end(); ++it)
		{
			/******if (!(*it)->isTableFull())
			{
				if((*it)->autoUserEnter(u))
				{
					(*it)->canStartGame();
					return true;
				}
			}*******/
		}
		return false;
	}

	bool GameRoom::enterUser(User *u)
	{ 
		u->setInRoom(this);
		return _roomCorePlayers->insertPlayer(u);	
	}

	bool GameRoom::outUser(User *u)
	{ 
		if (u == NULL)
			return false;
		/********if (u->getInTable() != NULL)
			u->getInTable()->onUserOut(u);*************/

		_roomCorePlayers->removePlayer(u); 
		if (u->getInRoom() != NULL)
			u->setInRoom(NULL);
		return true;
	}

}