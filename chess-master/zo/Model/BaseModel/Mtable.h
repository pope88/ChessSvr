#ifndef _ICoreTable_H_
#define _ICoreTable_H_
#include "Mplayer.h"

class ICoreTable
{
	public:
	virtual ~ICoreTable(){}
	/**
	   @brief	ȡ����Ϸ��������
	   @ret		0��Ϸ�ҳ� 1���ֳ� 2������
	*/
	virtual UInt8 getGameType() = 0;
	/**
	   @brief ȡ�����ڵ�ǰ������������Թ�
	*/
	virtual UInt8 getCurPlayerNum() = 0;
	/**
	   @brief ȡ��ָ����λ�ŵ����ָ��
	*/
	virtual ICorePlayer* getCorePlayer(UInt8 nChairID) = 0;
	/**
	   @brief ����������Ϸ״̬����������ó�δ����״̬
	*/
	virtual void endGame() = 0;
	/**
	   @brief	���÷��䶨ʱ����ÿ������ͬʱֻ����һ����ʱ��,
				�ص��ɹ���,��ʱ���Զ���ɾ��
	*/
	//virtual void startTimer(UInt32 nEvent, UInt8 chairId) = 0;
	/**
		@brief	ɾ�������ڶ�ʱ��
	*/
	//virtual void removeTimer() = 0;
	/**
	   @brief	���Թ���ҷ�����Ϣ
	   @param	cChairID	> 0 ָ����cChairIDλ�õ��Թ����, Ϊ-1ʱָ��ȫ���Թ����
	   @param	pData		��Ϣ����
	   @param	nLength		��Ϣ����
	   @param	bExclude	Ϊ��true��cChairID >0 ʱָ����cChairIDλ�������������Թ����
	*/
	//virtual void notifyVisitor(UInt8 cChairID, const UInt8* pData, UInt16 nLength, bool bExclude = false) = 0;
	/**
	   @brief	�����з�������ҷ���һ��������ʱ������Ϣ
	   @param	cChairID >= 0�ȴ����в�������� -1ȫ�����
	*/
	virtual void startClientTimer(UInt8 cChairID, UInt32 nPeriod) = 0;
	/**
	@brief ��ȡƽ̨��Ϸ�ҳ�˰�� (0 - 100)
	@note �ú����ѷ���
	*/
	//virtual int GetTaxRate() = 0;

	/**
	   @brief ���û�pid ��ȡ�û���Ϸ��
	   @return ���ؽ�����û�����Ϸ��
	*/
	virtual UInt32 changePlayerMoney(UInt32 nPID, int nVarMoney) = 0;

	/**
	    @brief ���ñ�����Ϸ��ע,������OnStartGame�ص�����������
	**/
	//virtual void setBaseScore(UInt32 nScore) = 0;
	
};

class ITable
{
public:
	virtual ~ITable(){}

	virtual void bindCoreTable2Table(ICoreTable* pTable) = 0;
	virtual void release() = 0;
	virtual void onGameStart() = 0;
	virtual void onEndGame() = 0;
	virtual void onUserJoinVisitor(IPlayer* pPlayer) = 0;
	/**
	   @brief ����Ҿ�Э��ͨ�����뿪��Ϸ
	*/
	//virtual void onUserArrangeLeave(IPlayer* pPlayer) = 0;
	/**
	   @brief �����ǿ���뿪��Ϸ
	*/
	virtual void onUserForceLeave(IPlayer* pPlayer) = 0;
	/**
		@brief	�������Ϸ�����ж���
	*/
	virtual void onUserDisconnection(IPlayer* pPlayer) = 0;
	/**
		@brief	����ڶ��ߺ�30�����ٴν�����Ϸ
	*/
	virtual void onUserReconnection(IPlayer* pPlayer) = 0;
	/**
	//   @brief �������(�Ƕ���,�Թ����)���뷿��ʱ�ص�
	//*/
	//virtual void onPlayerJoin(IPlayer* pPlayer) = 0;
	/**
	   @brief ������뿪����ʱ�ص�
	*/
	virtual void onPlayerLeave(IPlayer* pPlayer) = 0;
	
	/**
	   @brief �Ƿ������½��뷿����Ҽ�����һ����Ϸ
	*/
	virtual bool canJoinGame(){ return true; }

	virtual void onOperateAck(IPlayer *player, UInt8 opcode, int mchips = 0) = 0;
};

#endif