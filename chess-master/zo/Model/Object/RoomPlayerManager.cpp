#include "Config.h"
#include "RoomPlayerManager.h"
#include "../Object/User.h"

namespace Object
{
	void RoomPlayerManager::onHeartBit()
	{
		User *user = NULL;
		for (auto it = _roomCorePlayers.begin(); it != _roomCorePlayers.end(); ++it)
		{
			user = it->second;
			user->heartBit();
		}
	}

	bool RoomPlayerManager::insertPlayer(User *user)
	{
		_roomCorePlayers[user->id()] = user;
		return true;
	}

	bool RoomPlayerManager::removePlayer(User *user)
	{
		auto iter = _roomCorePlayers.find(user->id());
		if ( iter != _roomCorePlayers.end())
		{
			_roomCorePlayers.erase(iter);
			return true;
		}
		return false;
	}

	void RoomPlayerManager::broadcastRoom(Packet::Builder& builder, User *pExceptPlayer)
	{
		if (pExceptPlayer == NULL)
		{
			for ( auto iter = _roomCorePlayers.begin(); iter != _roomCorePlayers.end(); ++iter)
			{
				builder.send(iter->second);
			}
		}
		else
		{
			for ( auto iter = _roomCorePlayers.begin(); iter != _roomCorePlayers.end(); ++iter)
			{
				if (pExceptPlayer != iter->second)
				{
					builder.send(iter->second);
				}
			}
		}
	}
}
