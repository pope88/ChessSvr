#ifndef _CCARD_H_ 
#define _CCARD_H_

struct CCardsType
{
public:
	CCardsType()
	{
		m_nTypeBomb = 0;
		m_nTypeNum = 0;
		m_nTypeValue = 0;
	}
	void SetValue(int nBomb, int nNum, int nValue)
	{
		m_nTypeBomb = nBomb;
		m_nTypeNum = nNum;
		m_nTypeValue = nValue;
	}
public:
	int m_nTypeBomb;	// 0:����ը�� 1:��ը�� 2:Ӳը��
	int m_nTypeNum;		//�Ƶ�����
	int m_nTypeValue;	//�Ƶ�ֵ
};

class CCard
{
public:
	CCard():m_nValue(0), m_nColor(0) {}
	CCard(UInt8 c, UInt8 v) { m_nColor = c; m_nValue = v; }
	~CCard(){}
public:
	CCard(const CCard &srcCard)
	{
		if (this == &srcCard)
			return;
		m_nValue = srcCard.m_nValue;
		m_nColor = srcCard.m_nColor;
	}
	CCard& operator =(CCard &srcCard)
	{
		if (this == &srcCard)
			return *this;
		m_nValue = srcCard.m_nValue;
		m_nColor = srcCard.m_nColor;
		return *this;
	}

	bool operator == (CCard &srcCard)
	{
		return (m_nColor == srcCard.m_nColor && m_nValue == srcCard.m_nValue);
	}
public:
	UInt8 m_nValue;
	UInt8 m_nColor;
};

/*
struct CCard
{
public:
	CCard():m_nValue(0), m_nColor(0) {}
	CCard(UInt8 nColor, UInt8 nValue) : m_nColor(nColor), m_nValue(nValue) {}
public:
	CCard(const CCard &srcCard)
	{
		if(this == &srcCard)
			return;
		m_nColor = srcCard.color();
		m_nValue = srcCard.cvalue();
	}
	CCard& operator = (const CCard &srcCard)
	{
		if (this == &srcCard)
			return *this;
		m_nColor = srcCard.color();
		m_nValue = srcCard.cvalue();
		return *this;
	}
	bool operator == (const CCard &srcCard)
	{
		return (m_nColor == srcCard.color() && m_nValue == srcCard.cvalue());
	}
public:
	UInt8 color()
	{
		return m_nColor;
	}
	void setcolor(UInt8 c)
	{
		m_nColor = c;
	}
	UInt8 cvalue()
	{
		return m_nValue;
	}
	void setcvalue(UInt8 v)
	{
		m_nValue = v;
	}
private:
	UInt8 m_nColor;  //��ɫ,-1��ʾû�����⻨ɫ
	UInt8 m_nValue;  //��ֵ,-1��ʾ��û����
};*/

#endif