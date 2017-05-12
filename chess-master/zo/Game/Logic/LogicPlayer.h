#ifndef _LOGICPLAYER_H_
#define _LOGICPLAYER_H_
#include "Model/BaseModel/MPlayer.h"
#include "SsuObject.h"
//#include "../Common/PlayCard.h"
#include "../Common/Poker.h"
#include "../../Model/Object/User.h"

class LogicGameTable;
class LogicPlayer : public IPlayer
{
public:
	enum PS_LogicPlayerSTATE
	{
		PS_NONE,
		PS_PLAYER,
		PS_CANDIDATE,
		PS_GIVEUP,
	};
	enum DZOP
	{
		COMMONPLAYER = (1 << 0),
		BANKER = (1 << 1),
		SMALLBLIND = (1 << 2),
		BIGBLIND = (1 << 3),
	};
public:
	LogicPlayer();
	virtual ~LogicPlayer();

	void release();
	void bindUser2LogicPlayer(Object::User *user); 
	virtual void onOperateAck(UInt8 opcode, int mchips = 0);

	void newRound();

    inline void setLogicGameTable(LogicGameTable *bt) { m_pGameTable = bt; }

	UInt8 getStatus(){ return m_nStatus; }
	void setStatus(UInt8 nStatus) { m_nStatus = nStatus; }

	UInt8 getLogicPlayerStatus() { return logicPlayerStatus; }
	void setLogicPlayerStatus(UInt8 mStatus) { logicPlayerStatus = mStatus; }

	bool isRacing() { return m_nStatus == PS_PLAYER; }

	void getLogicPlayerCards(::ssu::Object &noti, bool bShow);

	//void getCard(Card& cCard);
	int getMoney() { return m_pCorePlayer->getMoney(); }

	int	getChairID()
	{
		return m_pCorePlayer->getChairId(); 
	}

	bool operator > (const LogicPlayer *pb)
	{
		return this->mPoker > pb->mPoker;
	}
	bool operator == (const LogicPlayer *pb)
	{
		return this->mPoker == this->mPoker;
	}

public:

	ICorePlayer* getCorePlayer() { return m_pCorePlayer; }
	void onPacketPickCard(const ::ssu::Object &ack) {}
	void onPacketFinishSendCard(const ::ssu::Object &ack) {}
public:
public:
	Poker mPoker;
private:
	ICorePlayer* m_pCorePlayer;		  
	LogicGameTable* m_pGameTable;		     
	UInt8 m_nStatus;				    
	UInt8 logicPlayerStatus;           


};

struct lessLogicPlayer 
{
	bool operator()(const LogicPlayer *lhs, const LogicPlayer *rhs)
	{
		return lhs > rhs;
	}
};

#endif