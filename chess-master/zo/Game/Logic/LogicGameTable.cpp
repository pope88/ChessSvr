#include "Config.h"
#include "LogicGameTable.h"
#include "../../Packet/Builder.h"
#include "../../Model/Object/User.h"
#include "LogicGameTable.h"
LogicGameTable::LogicGameTable():m_bRacing(false), m_cCurOpChair(0), m_cCurOpcode(0), m_nPlyNum(0), m_bNewRound(false), m_baseChips(0), m_bSmallBlind(false), m_bbigBlind(false), m_btimeOut(false), m_lowestChips(0), m_nCommonStage(0), m_nLastBigBlind(0), playerSmall(NULL), playerBig(NULL)
{

}

LogicGameTable::~LogicGameTable()
{

}

void LogicGameTable::onGameStart()
{
	NewRound();

	showPlayerStatus();

	dealing();

	autoOperateBlind();

	SetBaseMoney();

}

void LogicGameTable::NewRound()
{
	m_Poke.NewRound();
	m_vecCommonCards.clear();
	m_vecPoker.clear();
	m_bRacing = true;
	m_cCurOpChair = 0;
	m_cCurOpcode = 0;
	m_nPlyNum = 0;
	m_bNewRound = false;
	m_baseChips = 0;
	m_bSmallBlind = false;
	m_bbigBlind = false;
	m_btimeOut = false;
	m_lowestChips = 0;
	m_nCommonStage = 0;
	m_nLastBigBlind = 0;
	playerSmall = NULL;
	playerBig = NULL;

	int nChair = rand() % ePLYNUM;   //ѡׯ��
	for(int i = 0; i < ePLYNUM; ++i)
	{
		int nOtherChair = (nChair + i) % ePLYNUM;
		LogicPlayer* pPlayer = getPlayer(nOtherChair);
		LogicPlayer* pBanker = getPlayer(m_Poke.getBanker());
		if(pPlayer == NULL)
			continue;
		m_vecPoker.push_back(pPlayer);
		if(m_bNewRound || pBanker == NULL)
		{
			m_Poke.setBanker(nOtherChair);
			m_bNewRound = false;
		}
		pPlayer->newRound();
		pPlayer->setStatus(LogicPlayer::PS_PLAYER);
		++m_nPlyNum;
	}

	setGameScore();

	SvrStartGameNot();

}

void LogicGameTable::SvrStartGameNot()
{
	Packet::PlayerGameSart pgs;
	pgs.SetBasechips(m_baseChips);
	pgs.SetLowestchips(m_lowestChips);
	pgs.SetBosschairpos(m_Poke.getBanker());
	pgs.SetBigblindpos(m_Poke.getBigBlinder());
	pgs.SetSmallblindpos(m_Poke.getSmallBlinder());
	NotifyTable(pgs);
}

void LogicGameTable::dealing()
{
	UInt8 beginChair = 0;
	if (m_nPlyNum == 2)
	{
		beginChair = m_Poke.getBanker();
	}
	else
	{
		beginChair = getNextPlayer(m_Poke.getBanker())->getChairID();
	}


	for(int i = 0; i < ePLYNUM; ++i)
	{
		int nOtherChair = (beginChair + i) % ePLYNUM;
		LogicPlayer* pPlayer = getPlayer(nOtherChair);
		if(pPlayer != NULL && pPlayer->getStatus() == LogicPlayer::PS_PLAYER)
		{
		
			Packet::PlayerHandCards ph;
			std::vector<CCard> initCards;

			CCard c;
			m_Poke.getCard(c);
			initCards.push_back(c);
			Packet::card *cd = ph.AddCards();
			cd->SetCardvalue(c.m_nValue);
			cd->SetCardcolor(c.m_nColor);

			m_Poke.getCard(c);
			initCards.push_back(c);
			cd = ph.AddCards();
			cd->SetCardvalue(c.m_nValue);
			cd->SetCardcolor(c.m_nColor);

			pPlayer->mPoker.setCards(initCards);
			ph.send(static_cast<Object::User*>(pPlayer->getCorePlayer()));
		}
	}

	startTimer(eDEALING_EVENT, -1);

}


//auto small and big blind
void LogicGameTable::autoOperateBlind()
{

	if (m_nPlyNum == 2)
	{
		playerSmall  = getPlayer(m_Poke.getBanker());
	}
	else
	{
		playerSmall = getNextPlayer(m_Poke.getBanker());
	}
	if (playerSmall == NULL)
	{
		return;
	}

	bool bHaveSmallBlind = false;
	if (!m_bSmallBlind)  //Сäעδ��
	{
		if((playerSmall->getLogicPlayerStatus() & SMALLBLIND) == SMALLBLIND) //�����Сäע
		{
			bHaveSmallBlind = true;	
		}
		else  //û��Сä���²���
		{
			showPlayerStatus();
			for (int i = 0; i < ePLYNUM; ++i)
			{
				LogicPlayer* pp = getPlayer(i);
				if ((pp != NULL) && (pp->getLogicPlayerStatus() == LogicPlayer::PS_PLAYER) && ((pp->getLogicPlayerStatus() & SMALLBLIND) == SMALLBLIND))
				{
					playerSmall = pp;
					bHaveSmallBlind = true;
				}
			}
		}
	}
	
	playerBig = getNextPlayer(playerSmall->getChairID());
	if (playerBig == NULL)
	{
		return;
	}

	bool bHaveBigBlind = false;
	if (!bHaveBigBlind)  //Сäעδ��
	{
		if((playerBig->getLogicPlayerStatus() & BIGBLIND) == BIGBLIND) //�����Сäע
		{
			bHaveBigBlind = true;	
		}
		else  //û�д�ä���²���
		{
			showPlayerStatus();
			for (int i = 0; i < ePLYNUM; ++i)
			{
				LogicPlayer* pp = getPlayer(i);

				if ((pp != NULL) && (pp->getLogicPlayerStatus() == LogicPlayer::PS_PLAYER) && ((pp->getLogicPlayerStatus() & BIGBLIND) == BIGBLIND))
				{
					playerBig = pp;
					bHaveBigBlind = true;
				}
			}
		}
	}

	LogicPlayer *nextPlayer = getNextPlayer(playerBig->getChairID());
	if(bHaveSmallBlind && bHaveBigBlind)
	{
		Packet::PlayerBlindChips pbc;
		pbc.SetTableamount(m_Poke.getBaseChips() * 3);

		playerSmall->mPoker.setCurrentChips(m_Poke.getBaseChips());
		playerSmall->mPoker.setChips(m_Poke.getBaseChips());
		Packet::PlayerBlindChips::blindInfo *pb = pbc.AddBlindinfos();
		pb->SetChairid(playerSmall->getChairID());
		pb->SetChips(m_Poke.getBaseChips());
		pb->SetRemainchips(playerSmall->mPoker.getPlayerChips()-m_Poke.getBaseChips());

		playerBig->mPoker.setCurrentChips(m_Poke.getBaseChips() * 2);
		playerBig->mPoker.setChips(m_Poke.getBaseChips() * 2);
		pb = pbc.AddBlindinfos();
		pb->SetChairid(playerSmall->getChairID());
		pb->SetChips(m_Poke.getBaseChips() * 2);
		pb->SetRemainchips(playerSmall->mPoker.getPlayerChips()-m_Poke.getBaseChips() * 2 );

		NotifyTable(pbc);  //noitfy table

		sendOperateReq(nextPlayer);
	}

}


void LogicGameTable::onOperateAck(IPlayer *iplayer, UInt8 opcode, int mchips)
{
	LogicPlayer *player = static_cast<LogicPlayer *>(iplayer);
	if (!m_bRacing)
		return;

	if ((opcode & GIVEUP) == 0)
	{
		if (player->getChairID() != m_cCurOpChair)
		{
			return;
		}
	}
	if (player->getStatus() == LogicPlayer::PS_GIVEUP)
	{
		return;
	}

	if ((m_cCurOpcode & opcode) == 0)
	{
		return;
	}

	removeValTimer();

	Packet::PlayerOperateNot ponot;
	int mLeaveChips = player->mPoker.getPlayerChips() - player->mPoker.getChips();
	int requireChips = m_Poke.getCallChips() - player->mPoker.getCurrentChips();
	if (requireChips < 0)
		return;
	if ((opcode & ADDCHIPS) == ADDCHIPS)
	{
		if ((mchips < requireChips) && (mLeaveChips >= requireChips))
			return;
		if (mLeaveChips < mchips)
			return;

		if (mchips > requireChips)
		{
			m_Poke.setCallChips(mchips);
		}
		UInt32 curChips = player->mPoker.getCurrentChips();
		player->mPoker.setCurrentChips(mchips + curChips);
		player->mPoker.setChips(player->mPoker.getChips() + mchips);
		m_Poke.setTotalChips(m_Poke.getTotalChips() + mchips);


		ponot.SetChairid(player->getChairID());
		ponot.SetOpcode(opcode);
		ponot.SetCurrentchips(player->mPoker.getCurrentChips());
		ponot.SetLeavechips(mLeaveChips);
		ponot.SetTotalchips(m_Poke.getTotalChips());
		NotifyTable(ponot);
	}
	else if ((opcode & CALL) == CALL)
	{
		int requireChips = m_Poke.getCallChips() - player->mPoker.getCurrentChips();
		int curChips = player->mPoker.getCurrentChips();
		if (mchips < requireChips)
			return;

		player->mPoker.setCurrentChips(mchips + curChips);
		player->mPoker.setChips(player->mPoker.getChips() + mchips);
		m_Poke.setTotalChips(m_Poke.getTotalChips() + mchips);
		
		ponot.SetChairid(player->getChairID());
		ponot.SetOpcode(opcode);
		ponot.SetCurrentchips(player->mPoker.getCurrentChips());
		ponot.SetLeavechips(mLeaveChips);
		ponot.SetTotalchips(m_Poke.getTotalChips());
		NotifyTable(ponot);
	}
	else if ((opcode & GIVEUP) == GIVEUP)
	{
		player->setStatus(LogicPlayer::PS_GIVEUP);

		ponot.SetChairid(player->getChairID());
		ponot.SetOpcode(opcode);
		ponot.SetCurrentchips(0);
		ponot.SetLeavechips(mLeaveChips);
		ponot.SetTotalchips(m_Poke.getTotalChips());
		NotifyTable(ponot);
		--m_nPlyNum;
	}
	else
	{
		return;
	}

	if (m_nPlyNum == 1)
	{
		roundEnd();
		return;
	}

	if ( (player->getChairID() == m_Poke.getBanker() 
		|| (isBossGiveUp() && player == getNextPlayer(m_Poke.getBanker()) ) )
		&& isCanSendCommonCard())
	{
		sendCommonCards();
		m_Poke.setCallChips(0);
	}

	LogicPlayer *pp = getNextPlayer(player->getChairID());
	if (pp != NULL)
	{
		sendOperateReq(pp);
	}
}

void LogicGameTable::sendOperateReq(LogicPlayer *player)
{
	Packet::PlayerOperateReq poReq;
	poReq.SetChairid(player->getChairID());
	poReq.SetBasechips(m_Poke.getBaseChips());
	poReq.SetCurrentchips(m_Poke.getCallChips() - player->mPoker.getCurrentChips());

	if (poReq.Currentchips() > (player->mPoker.getPlayerChips() - player->mPoker.getChips()))
	{
		onOperateAck(player, GIVEUP);
	}
	else
	{
		UInt32 opcode = 0;
		if (m_Poke.getCallChips() > 0)
		{
			opcode |= GIVEUP;
			opcode |= CALL;
			opcode |= ADDCHIPS;
			poReq.SetOpcode(opcode);
		}
		else
		{
			opcode |= CHECK;
			opcode |= ADDCHIPS;
		}
		poReq.SetOpcode(opcode);

		m_cCurOpChair = player->getChairID();
		m_cCurOpcode = opcode;

		// m_pCoreTable->removeTimer();

		NotifyTable(poReq);
		startTimer(eBET_EVENT, m_cCurOpChair);
	}
}


void LogicGameTable::showPlayerStatus()
{
	int nChair = -1;
	LogicPlayer *p = getPlayer(m_Poke.getBanker());
	if (p != NULL && (p->getStatus() == LogicPlayer::PS_PLAYER || p->getStatus() == LogicPlayer::PS_GIVEUP))
	{	
		nChair = m_Poke.getBanker();
	}
	else
	{
		LogicPlayer* pp = getAfterPlayer(m_Poke.getBanker());
		if (pp != NULL)
		{
			nChair = pp->getChairID();
			m_Poke.setBanker(nChair);
		}
	}

	int nNum = 0;
	for (int i = 0; i < ePLYNUM; ++i)
	{
		LogicPlayer *pp = getPlayer(i);
		if ((pp != NULL) && (pp->getStatus() == LogicPlayer::PS_PLAYER || pp->getStatus() == LogicPlayer::PS_GIVEUP))
		{
			++nNum; 
		}
	}
	
	Packet::PlayerGameStatus pgs;
	Packet::PGStatus *ps = pgs.AddPlayerstatus();
	for (int i = 0; i < ePLYNUM; ++i)
	{
		LogicPlayer* pPlayer = getPlayer(nChair);
		if (pPlayer== NULL)
		{
			++nChair;
			nChair = (nChair)%ePLYNUM;
			continue;
		}
		if (pPlayer->getStatus() == LogicPlayer::PS_CANDIDATE || pPlayer->getStatus() == LogicPlayer::PS_NONE)
		{
			++nChair;
			nChair = (nChair)%ePLYNUM;
			continue;
		}	

		
		ps->SetChairid(nChair);
		if (nNum == 2)
		{
			if (nChair == m_Poke.getBanker())
			{
				pPlayer->setLogicPlayerStatus(LogicPlayer::BANKER|SMALLBLIND);
			    ps->SetStatus(LogicPlayer::BANKER|LogicPlayer::SMALLBLIND);
			}
			else if (getBeforePlayerID(nChair) == m_Poke.getBanker())
			{
				pPlayer->setLogicPlayerStatus(BIGBLIND);
			    ps->SetStatus(BIGBLIND);
			}
		}
		else
		{
			if (nChair == m_Poke.getBanker())
			{
				pPlayer->setLogicPlayerStatus(LogicPlayer::BANKER);
				ps->SetStatus(LogicPlayer::BANKER);
			}
			else if (getBeforePlayerID(nChair) == m_Poke.getBanker())
			{
				pPlayer->setLogicPlayerStatus(SMALLBLIND);
				ps->SetStatus(LogicPlayer::SMALLBLIND);
			}
			else if (getBeforePlayerID(getBeforePlayerID(nChair)) == m_Poke.getBanker())
			{
				pPlayer->setLogicPlayerStatus(BIGBLIND);
				ps->SetStatus(LogicPlayer::BIGBLIND);
			}
			else
			{
				pPlayer->setLogicPlayerStatus(LogicPlayer::COMMONPLAYER);
				ps->SetStatus(LogicPlayer::COMMONPLAYER);
			}
		}

		++nChair;
		nChair = (nChair)%ePLYNUM;
	}
	NotifyTable(pgs);
}


void LogicGameTable::sendCommonCards()
{
	std::vector<CCard> cards;

	++m_nCommonStage;
	if (m_nCommonStage == 1)
	{
		for (size_t i = 0; i < 3; ++i)
		{
			CCard c;
			m_Poke.getCard(c);
			m_vecCommonCards.push_back(c);
		}
		cards = m_vecCommonCards;
	}
	else if (m_nCommonStage == 2 || m_nCommonStage == 3)
	{
		CCard c;
		m_Poke.getCard(c);
		cards.push_back(c);
	}

	Packet::PlayerCommonCards pcc;
	pcc.SetStep(m_nCommonStage);
	for (size_t i = 0; i < cards.size(); ++i)
	{
		Packet::card *pc;
		pc = pcc.AddCards();
		pc->SetCardvalue(cards[i].m_nValue);
		pc->SetCardcolor(cards[i].m_nColor);
	}
	NotifyTable(pcc);
}

void LogicGameTable::onPlayerJoin(IPlayer* pPlayer)
{
	if (pPlayer != NULL)
	{
		static_cast<LogicPlayer*>(pPlayer)->setLogicGameTable(this);
	}
}

void LogicGameTable::onPlayerLeave(IPlayer* pPlayer)
{
	if (pPlayer == NULL)
	{
		return;
	}
	LogicPlayer* player = static_cast<LogicPlayer*>(pPlayer);

	player->setLogicGameTable(NULL);
	
	player->setStatus(LogicPlayer::PS_NONE);

	if(!m_bRacing)
		return;

	//notify player out
	Packet::UserLeaveTable ul;
	ul.SetChairid(player->getChairID());
	NotifyTable(ul, 0, player);

	--m_nPlyNum;

	//if end ,end
	if (m_nPlyNum == 1)
	{
		roundEnd();
	}
}

void LogicGameTable::startTimer(int nEvent, char cChair)
{
	UInt16  time = 0;
	if (nEvent == eBET_EVENT)
	{
		this->addInterValTimer(eBET_EVENT, eBET_PERIOD);
		time = eBET_PERIOD; 
	}
	else if (nEvent == ePICK_EVENT)
	{
		this->addInterValTimer(ePICK_EVENT, ePICK_PERIOD);
		time = ePICK_PERIOD;
	}
	else if(nEvent == eDEALING_EVENT)
	{
		this->addInterValTimer(eDEALING_EVENT, eDEALING_PERIOD);
		time = eDEALING_PERIOD;
	}

	printf("Start Timer Event-%d---time-%d---chairid-%d---\n", nEvent, time, cChair);
	//timer
	m_pCoreTable->startClientTimer(cChair, time);
}

void LogicGameTable::onTimer()
{
	UInt32 timeid = timerId();
	printf("On Timer---%d--\n", timeid);
	switch (timerId())
	{
	case eBET_EVENT:
		{
			if (!m_bRacing)
				return;

			LogicPlayer *player = getPlayer(m_cCurOpChair);
			if (player == NULL)
				return;

			if (player->getStatus() == LogicPlayer::PS_GIVEUP)
				return;
			
			UInt8 chairId = 0;
			chairId = m_cCurOpChair;
			LogicPlayer *curPlayer = getPlayer(chairId);
			if (curPlayer == NULL)
			{
				return;
			}
			else
			{
				if ((m_cCurOpcode & CHECK) == CHECK)
				{
					onOperateAck(curPlayer, CHECK, 0);
				}
				else
				{
					onOperateAck(curPlayer, GIVEUP, 0);
				}
			}
		}
		break;
	case ePICK_EVENT:
		{
		}
		break;
	case eDEALING_EVENT:
		{

		}
		break;
	default:
		break;
	}
}


UInt8 LogicGameTable::getBeforePlayerID(UInt8 nChairID)
{
	int nTemp = 0;
	for (int i = 1; i <= ePLYNUM+1; ++i)
	{
		nTemp = (nChairID - i + ePLYNUM)%ePLYNUM;
		LogicPlayer* p = getPlayer(nTemp);
		if (p && (p->getStatus() == LogicPlayer::PS_PLAYER || p->getStatus() == LogicPlayer::PS_GIVEUP))
		{
			return p->getChairID();
		}
	}
	return nChairID;
}

LogicPlayer* LogicGameTable::getNextPlayer(UInt8 nChairID)
{
	UInt8 chairid = nChairID;
	for(int i = 0; i < ePLYNUM; ++i)
	{
		++chairid;
		UInt8 cNextChair = chairid % ePLYNUM;
		LogicPlayer *p = getPlayer(cNextChair);
		if(p && p->getStatus() == LogicPlayer::PS_PLAYER)
			return p;
	}
	return NULL;
}


LogicPlayer* LogicGameTable::getAfterPlayer(UInt8 nChairID)
{
	UInt8 chairid  = nChairID;
	for (UInt8 i = 0; i < ePLYNUM; ++i)
	{
		++chairid;
		UInt8 nNextChair = chairid % ePLYNUM;
		LogicPlayer *player = getPlayer(nNextChair);
		if (player != NULL && (player->getStatus() == LogicPlayer::PS_PLAYER || player->getStatus() == LogicPlayer::PS_GIVEUP ) )
			return player;
	}
	return NULL;
}

bool LogicGameTable::isBossGiveUp()
{
	LogicPlayer *player = getPlayer(m_Poke.getBanker());
	if (player == NULL || player->getStatus() == LogicPlayer::PS_GIVEUP)
	{
		return true;
	}
	return false;
}

bool LogicGameTable::isCanSendCommonCard()
{
	for (int i = 0; i < ePLYNUM; ++i)
	{
		LogicPlayer* p = getPlayer(i);
		if (p != NULL && p->getStatus() == LogicPlayer::PS_PLAYER)
		{
			if (p->mPoker.getCurrentChips() != m_Poke.getCallChips() && p->mPoker.getChips() > 0)
			{
				return false;
			}
		}
	}
	return true;
}

void LogicGameTable::roundEnd()
{
	m_pCoreTable->endGame();

	printf("-----round end--------\n");
	m_bRacing = false;

	removeValTimer();
	//for (int i = 0; i < ePLYNUM; ++i)
	//{
	//	LogicPlayer* pp = getPlayer(i);
	//	if ((pp != NULL) && (pp->getStatus() == LogicPlayer::PS_PLAYER))
	//	{
	//		m_vecPoker.push_back(pp);
	//	}
	//}

	for (int i = 0; i < ePLYNUM; ++i)
	{
		LogicPlayer* pp = getPlayer(i);
		if ((pp != NULL) && (pp->getStatus() != LogicPlayer::PS_NONE))
		{
			//sort(m_vecPoker.begin(), m_vecPoker.end(), lessPlayer());
			Packet::EndRound er;
			er.send(static_cast<Object::User*>(pp->getCorePlayer()));
		}
	}
}


void LogicGameTable::SendCompleteData(LogicPlayer* pPlayer)
{
	//pt_dz_completedata_not noti;
}

LogicPlayer* LogicGameTable::getPlayer(UInt8 nChairID)
{
    ICorePlayer* pCorePlayer = m_pCoreTable->getCorePlayer(nChairID);
	if(pCorePlayer != NULL)
		return (LogicPlayer*)((Object::User*)pCorePlayer)->getThePlayer();
	return NULL;
}

void LogicGameTable::sendPlayerCard()
{
	/*
	std::vector<CCard> vecCards;
	//pt_dz_sendcard_not noti;
	//noti.opcode = dz_sendcard_not;
	if (m_nPlyNum == 2)
	{
		//noti.cBeginChair = m_Poke.m_nBanker;
	}
	else
	{
		Player* pp = getNextPlayer(m_Poke.m_nBanker);
		if (pp)
		{
			//noti.cBeginChair = pp->getChairID();
		}
	}

	int nBegin = 0;
	//int nBegin = noti.cBeginChair;
	for(int i = 0; i < ePLYNUM; ++i)   //�Ȼ��������Ϸ�����λ��
	{
		Player* player = getPlayer((i+ nBegin)%ePLYNUM);
		if(player && player->getStatus() == Player::PS_PLAYER)
		{
			//noti.vecChairs.push_back((i+nBegin)%ePLYNUM);
			vecCards.clear();
			for (int j = 0; j < 2; ++j)
			{
				vecCards.push_back(m_Poke.GetCard());
			}
			player->mPoker.setCards(vecCards);
		}
	}
	
	int nChair = 0;
	//int nChair = m_Poke.m_nBanker;
	--nChair;
	nChair = (ePLYNUM + nChair)%ePLYNUM;
	for (int i = 0; i < ePLYNUM; ++i)  //����
	{
		Player* pPlayer = getPlayer(nChair);
		if(pPlayer && pPlayer->getStatus() == Player::PS_PLAYER)
		{
			pPlayer->mPoker.getVecCards(noti.vecCards);
			pPlayer->sendPacket(noti);
			if (pPlayer->CanWatch())
			{
				NotifyVistor(nChair, noti);
			}
			else
			{
				st_card EmptyCard;
				vector<st_card> temp;
				temp.push_back(EmptyCard);
				temp.push_back(EmptyCard);
				noti.vecCards  = temp;
				NotifyVistor(nChair, noti);
			}

		}
		--nChair;
		nChair = (ePLYNUM + nChair)% ePLYNUM;
	}
	m_pCoreTable->RemoveTimer();
	StartTimer(eDEALING_EVENT,-1);*/
}

void LogicGameTable::onFinishSendAck(LogicPlayer* p)
{
	/*
	if (!p)
	{
		return;
	}

	if (!p->mPoker.getFinishCard())
	{
		p->mPoker.setFinishCard(true);
		++m_Poke._finishNum++;
	}
	else
	{
		return;
	}

	bool bBegin = true;
	for (int i = 0; i < ePLYNUM; ++i)
	{
		Player *p = getPlayer(i);
		if (p && (p->getStatus() == Player::PS_PLAYER) && !p->mPoker.getFinishCard())
		{
			bBegin = false;
			break;
		}
	}
	if (bBegin)
	{
		Player *player  = NULL;
		if (m_nPlyNum == 2)
		{
			player  = getPlayer(m_Poke.getBanker());
		}
		else
		{
			player = getNextPlayer(m_Poke.getBanker());
		}
		if (!player)
		{
			return;
		}
		sendOperateReq(player, 1);
	}
	*/
}

