#include "Config.h"
#include "GameTable.h"
#include "ZoGlobal.h"
#include "User.h"
#include "Model/BaseModel/ServerModule.h"
#include "../../Packet/Builder.h"
#include "Game/Logic/LogicGameTable.h"

namespace Object
{
	GameTable::GameTable(UInt16 tableId, RoomPlayerManager* rpManager):tableId(0), roomPlayerManager(rpManager), maxPlayerNum(zoGlobal.playerNum()), curPlayerNum(0), timerId(0), masterChairId(0), baseScore(0), kickChairId(0), m_pTable(NULL){
		vecPlayers.resize(maxPlayerNum);
		for ( auto it = vecPlayers.begin(); it != vecPlayers.end(); ++it) {
			*it = NULL;
		}
		m_pTable = gpServerModule::instance()->createTable();
		m_pTable->bindCoreTable2Table(this);
	}

	ICorePlayer* GameTable::getCorePlayer(UInt8 nChairId) {
		if (nChairId > maxPlayerNum) {
			return NULL;
		}
		return vecPlayers[nChairId];
	}

	void GameTable::endGame() {
		for (auto it = vecPlayers.begin(); it != vecPlayers.end(); ++it) {
			if (NULL == *it) {
				continue;
			}
			(*it)->setPlayerStatus(User::US_RACING);

			if ((*it)->getMoney() < zoGlobal.getGameMoneyLimit()) {
				// ��ͻ��˷���Ϣ,��Ϊ������Ϸ�Ҳ��㣬�Ѿ��������뿪�÷���!�����ֵ<��������
				leaveTable(*it);
			}
		}
	}

	//void GameTable::startTimer(UInt32 nEvent, UInt8 chairId) {

	//}
	/**
	* tags����ͻ��˷��Ϳ�ʼ��ʱ������Ϣ
	*/
	void GameTable::startClientTimer(UInt8 cChairID, UInt32 nPeriod) {

	}

	/**
	  * tags�����������Ϸ��
	 */
	UInt32 GameTable::changePlayerMoney(UInt32 userId, int nMoney) {
		return 0;
	}

	bool GameTable::leaveTable(User *pUser) {

		// ����Ϣ֪ͨ����뿪
		if (pUser->getStatus() == User::US_RACING) {
			pUser->setLeaveTable(true);
		}
		return true;
	}
}