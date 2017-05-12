#ifndef _ICoreTable_H_
#define _ICoreTable_H_
#include "Mplayer.h"

class ICoreTable
{
	public:
	virtual ~ICoreTable(){}
	/**
	   @brief	取得游戏房间类型
	   @ret		0游戏币场 1积分场 2比赛场
	*/
	virtual UInt8 getGameType() = 0;
	/**
	   @brief 取房间内当前玩家数，不算旁观
	*/
	virtual UInt8 getCurPlayerNum() = 0;
	/**
	   @brief 取得指定坐位号的玩家指针
	*/
	virtual ICorePlayer* getCorePlayer(UInt8 nChairID) = 0;
	/**
	   @brief 结束房间游戏状态，所有玩家置成未举手状态
	*/
	virtual void endGame() = 0;
	/**
	   @brief	设置房间定时器，每个房间同时只能有一个定时器,
				回调成功后,定时器自动被删除
	*/
	//virtual void startTimer(UInt32 nEvent, UInt8 chairId) = 0;
	/**
		@brief	删除房间内定时器
	*/
	//virtual void removeTimer() = 0;
	/**
	   @brief	向旁观玩家发送消息
	   @param	cChairID	> 0 指定仅cChairID位置的旁观玩家, 为-1时指定全体旁观玩家
	   @param	pData		消息内容
	   @param	nLength		消息长度
	   @param	bExclude	为真true且cChairID >0 时指定除cChairID位置外其它所有旁观玩家
	*/
	//virtual void notifyVisitor(UInt8 cChairID, const UInt8* pData, UInt16 nLength, bool bExclude = false) = 0;
	/**
	   @brief	向所有房间内玩家发送一个启动定时器的消息
	   @param	cChairID >= 0等待进行操作的玩家 -1全体玩家
	*/
	virtual void startClientTimer(UInt8 cChairID, UInt32 nPeriod) = 0;
	/**
	@brief 获取平台游戏币场税率 (0 - 100)
	@note 该函数已废弃
	*/
	//virtual int GetTaxRate() = 0;

	/**
	   @brief 按用户pid 存取用户游戏币
	   @return 返回结算后用户的游戏币
	*/
	virtual UInt32 changePlayerMoney(UInt32 nPID, int nVarMoney) = 0;

	/**
	    @brief 设置本局游戏底注,建议在OnStartGame回调函数中设置
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
	   @brief 有玩家经协议通过后离开游戏
	*/
	//virtual void onUserArrangeLeave(IPlayer* pPlayer) = 0;
	/**
	   @brief 有玩家强制离开游戏
	*/
	virtual void onUserForceLeave(IPlayer* pPlayer) = 0;
	/**
		@brief	有玩家游戏进行中断线
	*/
	virtual void onUserDisconnection(IPlayer* pPlayer) = 0;
	/**
		@brief	玩家在断线后30秒内再次进入游戏
	*/
	virtual void onUserReconnection(IPlayer* pPlayer) = 0;
	/**
	//   @brief 有新玩家(非断线,旁观玩家)进入房间时回调
	//*/
	//virtual void onPlayerJoin(IPlayer* pPlayer) = 0;
	/**
	   @brief 有玩家离开房间时回调
	*/
	virtual void onPlayerLeave(IPlayer* pPlayer) = 0;
	
	/**
	   @brief 是否允许新进入房间玩家加入下一轮游戏
	*/
	virtual bool canJoinGame(){ return true; }

	virtual void onOperateAck(IPlayer *player, UInt8 opcode, int mchips = 0) = 0;
};

#endif