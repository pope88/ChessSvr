#include "Config.h"
#include "TableCreator.h"
#include "Game/Logic/LogicGameTable.h"

ITable* TableCreator::CreateTable()
{
	ITable *ptable = new(std::nothrow) LogicGameTable();
	return ptable;
}