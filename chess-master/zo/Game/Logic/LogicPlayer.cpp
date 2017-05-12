#include "Config.h"
#include "LogicPlayer.h"
#include "LogicGameTable.h"


LogicPlayer::LogicPlayer() : m_pCorePlayer(NULL), /*m_pGameTable(NULL),*/ m_nStatus(PS_NONE), logicPlayerStatus(COMMONPLAYER)
{
}

LogicPlayer::~LogicPlayer()
{
}

void LogicPlayer::bindUser2LogicPlayer(Object::User *user)
{
	m_pCorePlayer = user;
	m_pGameTable = NULL;
}

void LogicPlayer::onOperateAck(UInt8 opcode, int mchips)
{
	m_pGameTable->onOperateAck(this, opcode, mchips);
}

void LogicPlayer::release()
{
	m_pGameTable = NULL;
	m_pCorePlayer = NULL;
	delete this;
}

void LogicPlayer::newRound()
{
	mPoker.newRound();
	m_nStatus = PS_NONE;
	logicPlayerStatus = COMMONPLAYER;
}


//void LogicPlayer::getCard(Card& cCard)
//{
//	m_PlayCard.m_cCards.push_back(cCard);
//}

void LogicPlayer::getLogicPlayerCards(::ssu::Object &noti, bool bShow)
{
	//noti.opcode = Svr_RefreshCard_Not;
	//noti.cChairID = m_pCorePlayer->GetChairID();
	//noti.vecCards = m_PlayCard.m_cCards;

	////œ‘ æ≈∆√Ê
	//if( !bShow && !m_bShowCard )
	//{
	//	for(size_t i = 0; i < noti.vecCards.size(); i++)
	//	{
	//		Card& cCard = noti.vecCards[i];
	//		cCard.m_nColor = 0;
	//		cCard.m_nValue = 0;
	//	}
	//}
}


/*
void LogicPlayer::ProcessPacket(const char* pData, int nLen)
{
	CInputStream is(pData, nLen);
	short opcode;
	is >> opcode;
	is.Reset();

	switch(opcode)
	{
	case Cli_SendCardOK_Ack:	DISPATCH_MESSAGE(pt_Cli_SendCardOK_Ack);	break;
	case Cli_CallScore_Ack:		DISPATCH_MESSAGE(pt_Cli_CallScore_Ack);		break;
	case Cli_PlayCard_Ack:		DISPATCH_MESSAGE(pt_Cli_PlayCard_Ack);		break;
	case Cli_RobLord_Ack:		DISPATCH_MESSAGE(pt_Cli_RobLord_Ack);		break;
	case Cli_Auto_Req:			DISPATCH_MESSAGE(pt_Cli_Auto_Req);			break;
	case Cli_CompleteData_Req:	DISPATCH_MESSAGE(pt_Cli_CompleteData_Req);	break;
	case Cli_Voice_Req:			DISPATCH_MESSAGE(pt_Cli_Voice_Req);			break;
	case Cli_ShowCard_Ack:		DISPATCH_MESSAGE(pt_Cli_ShowCard_Ack);		break;
	default:
		g_pLogger->Log("Unknow Cmd %d", opcode);		
		break;
	}
}

void LogicPlayer::OnPacket(const pt_Cli_SendCardOK_Ack& ack)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_pGameTable->CliSendCardAck(ack, this);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_CallScore_Ack& ack)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_pGameTable->CliCallScoreAck(ack, this);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_RobLord_Ack& ack)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_pGameTable->CliRobLordAck(ack, this);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_PlayCard_Ack& ack)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_pGameTable->CliPlayCardAck(ack, this);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_Auto_Req& req)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_bAuto = req.cAuto;

		pt_Svr_Auto_Not noti;
		noti.opcode = Svr_Auto_Not;
		noti.cChairID = GetChairID();
		noti.cAuto = req.cAuto;
		m_pGameTable->NotifyRoom(noti);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_CompleteData_Req& req)
{
	if( m_pGameTable )
	{
		m_pGameTable->GetCompleteData(this);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_Voice_Req& req)
{
	if( m_pGameTable )
	{
		m_pGameTable->NotifyRoom(req);
	}
}

void LogicPlayer::OnPacket(const pt_Cli_ShowCard_Ack& ack)
{
	if( m_pGameTable && m_pGameTable->m_bRacing )
	{
		m_pGameTable->CliShowCardAck(ack, this);
	}
}
*/