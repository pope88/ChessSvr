#include "Config.h"
#include "PlayerCreator.h"
#include "../Logic/LogicPlayer.h"

IPlayer* PlayerCreator::CreatePlayer()
{
	return new(std::nothrow) LogicPlayer();
}