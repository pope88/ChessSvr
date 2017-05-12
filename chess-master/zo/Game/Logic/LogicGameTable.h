#ifndef _LOGICGAMETABLE_H_
#define _LOGICGAMETABLE_H_
#include "Model/BaseModel/Mplayer.h"
#include "Model/BaseModel/Mtable.h"
#include "SsuObject.h"
#include "Poke.h"
#include "../../Model/Object/TimerBase.h"
#include "../../Packet/Builder.h"
#include "../Common/CCard.h"
#include "LogicPlayer.h"

class LogicGameTable : public ITable, public TimerBase
{
	public:
	enum TIMER_ID
	{
		eBET_EVENT = 1,
		ePICK_EVENT = 2,
		eDEALING_EVENT = 3,
		eCOMMONCARD_EVENT = 4,
		eCARDFACE_EVENT = 5,
		ePICK_PERIOD = 30,
		eBET_PERIOD = 30,
		eDEALING_PERIOD = 5,
		eCARDFACE_PERIOD = 2,
	};

	enum 
	{
		eALLUSER = 0,
		eONLYPLAYER = 1,
		ePLYNUM = 7,
	};

	enum DZPKOP
	{
		GIVEUP =     (1 << 0),
		CALL =       (1 << 1),
		CHECK =      (1 << 2),
		ADDCHIPS =   (1 << 3),
		SMALLBLIND = (1 << 4),
		BIGBLIND =   (1 << 5),
	};

public:
	LogicGameTable();
	~LogicGameTable();
	virtual void bindCoreTable2Table(ICoreTable* pTable) { m_pCoreTable = pTable; }
	virtual void release() { delete this; }
	virtual void onTimer();
	virtual void onGameStart();
	virtual void onEndGame() {}
	//void onPacketOperate(const ::ssu::Object &ack) {}
	void onPacketPickCard(const ::ssu::Object &ack ) {}
	void onPacketFinishSendCard(const ::ssu::Object &ack) {}
	/**
	   @brief �����ڹ㲥��Ϣ
	   @param packet ���ݰ� 
	   @param nType 0ȫ����㲥, 1��ҹ㲥, 2ȫ���Թ۹㲥
	   @param pExceptPlayer �ų��������
	*/
	
	void NotifyTable(Packet::Builder& pb, int nType = 0, LogicPlayer* pExceptPlayer = NULL)
	{
		for (UInt8 i = 0; i < ePLYNUM; ++i)
		{
			ICorePlayer *player = m_pCoreTable->getCorePlayer(i);
			if ( player == NULL || (pExceptPlayer != NULL && pExceptPlayer->getCorePlayer() == player))
			{
				continue;
			}
			pb.send((Object::User*)player);
		}
	}	
public:
	//��ʼ��Ϸ
	void NewRound(); 
	//����һ����ʱ��
	void startTimer(int nEvent, char cChair);
	void showPlayerStatus();
	void dealing();
	inline UInt32 getBaseChips() { return m_baseChips; }
	UInt8 getBeforePlayerID(UInt8 nChairID);
	LogicPlayer* getNextPlayer(UInt8 nChairID);
	LogicPlayer* getPlayer(UInt8 nChairID);
	LogicPlayer* getAfterPlayer(UInt8 nChairID);
	bool isBossGiveUp();
	bool isCanSendCommonCard();
	void SendCompleteData(LogicPlayer* pPlayer);
	void sendPlayerCard();

	void onFinishSendAck(LogicPlayer* p);
	void sendOperateReq(LogicPlayer *player);

	void onOperateAck(IPlayer *iplayer, UInt8 opcode, int mchips = 0);

	void autoOperateBlind();
	//��Ϸ����
	void roundEnd();
	void sendCommonCards();

	void onPlayerJoin(IPlayer* pPlayer);

public:
	void setGameScore() { m_baseChips = 100; m_lowestChips = 10000; m_Poke.setCallChips(m_baseChips * 2 ); }

	//�ͻ��˻�Ӧ�������
	void cliSendCardAck(const ::ssu::Object &ack, LogicPlayer* pPlayer) {}


	void cliPickCardAck(const ::ssu::Object &ack, LogicPlayer* pPlayer) {}

	void CliOperatorAck(const ::ssu::Object &ack, LogicPlayer* pPlayer) {}


	//��ȡ��Ϸ���ݣ��Թۡ�����������
	void GetCompleteData(LogicPlayer *pPlayer) {}

	//����ͨ����Ϣ
	void SendCommonCmd(int nOp, int nChairID = -1) {}

	//virtual void onUserArrangeLeave(IPlayer* pPlayer) {}
	virtual void onUserForceLeave(IPlayer* pPlayer) {}
	virtual void onUserDisconnection(IPlayer* pPlayer) {}
	virtual void onUserReconnection(IPlayer* pPlayer) {}
	virtual void onUserJoinVisitor(IPlayer* pPlayer) {}
	void onPlayerLeave(IPlayer* pPlayer);
	virtual bool canJoinGame() { return true; }

	
	//��ʼ��Ϸ�㲥
	void SvrStartGameNot();

	//���õ�ע
	void SetBaseMoney() {}

	/**
	@brief �쳣������Ϸ
	@param nType 0Э���˳� 1ǿ���˳�
	@param pPlayer ǿ���˳����
	*/
	void AbnormalEndGame(int nType, ICorePlayer *pPlayer) {}

	/**
	@brief	��ͻ���ˢ��ĳ���������Ϣ
	@param	pPalyer ���淢���仯�����
	@param	pExceptPlayer �ų�����Ҳ���
	@param	bPlyShow �Ƿ�����Ϸ�߷�������	
	@param	bShowAll �Ƿ����Թ��߷�������	
	*/
	void RefreshCards(LogicPlayer* pPlayer, LogicPlayer* pExceptPlayer = NULL, bool bPlyShow = false, bool bShowAll = false) {}

private:
	ICoreTable* m_pCoreTable;					//ϵͳ�ӿ�
	Poke m_Poke;								//��
	bool m_bRacing;                             //����״̬
	std::vector<CCard> m_vecCommonCards;        //������
	std::vector<LogicPlayer*> m_vecPoker;            //players
	UInt8 m_cCurOpChair;         //��ǰ�ȴ�������λ��
	UInt8 m_cCurOpcode;          //��ǰ�ȴ��Ĳ���
	UInt8 m_nPlyNum;             //����ʱ���������
	bool m_bNewRound;            //�Ƿ���Ҫѡׯ
	UInt32 m_baseChips;          //��Ϸ��ע
	bool m_bSmallBlind;         //Сäע����
	bool m_bbigBlind;            //��äע����
	bool m_btimeOut;             //�Ѿ���ʱ
	UInt32 m_lowestChips;         //��ͳ�������
	UInt8 m_nCommonStage;          //�����Ʒ��Ʋ���
	UInt8 m_nLastBigBlind;       //��һ�δ�äעchairid
	LogicPlayer *playerSmall;
	LogicPlayer *playerBig;

};

#endif