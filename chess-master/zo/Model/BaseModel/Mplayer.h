#ifndef _ICorePlayer_H_
#define _ICorePlayer_H_
namespace Object
{
	class User;
}

class IPlayer
{
public:
	virtual ~IPlayer(){}
	virtual void bindUser2LogicPlayer(Object::User *user) = 0;
	virtual void onOperateAck(UInt8 opcode, int mchips = 0) = 0;
};

class ICorePlayer
{
public:
	ICorePlayer() {}	
	virtual ~ICorePlayer() {}
public:
	virtual int getTableId() = 0;
	virtual void setTableId(int tableid) = 0;
	virtual int getChairId() = 0;
	virtual void setChairId(int chairid) = 0;
	virtual UInt32 getMoney() = 0;
	virtual bool saveGameMoney(int money) = 0;
	virtual bool saveGameScore(int score) = 0;
	virtual bool canWatch() = 0;
};




#endif