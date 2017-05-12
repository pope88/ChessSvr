#ifndef _ZOGLOBAL_H_
#define _ZOGLOBAL_H_

class ZoGlobal
{
public:
	enum READYMODE
	{
		MODEZERO = 0, //satisfy users full and all users hands up 
		MODEONE = 1,  //satisfy at lease two users and they all hands up
		MODETWO = 2,  //satisfy at lease three users and they all hands up
		MODETHREE = 3, //satisfy at lease four users and they all hands up
		MODELAST = 4,  //satisfy at lease two users 
	};
public:
	void init();
	inline UInt8 roomNum() { return _roomNum; }
	inline void roomNum(UInt8 r)  { _roomNum = r; }
	inline UInt16 tableNum() { return _tableNum;}
	inline void tableNum(UInt16 t) { _tableNum = t; }
	inline UInt8 playerNum() { return _playerNum; } 
	inline void playerNum(UInt8 pn) { _playerNum = pn; }
	inline UInt8 getReadyMode() { return _readyMode; }
	inline UInt8 getGameType() { return _gameType; }
	inline UInt32 getGameMoneyLimit() { return _gameMoneyLimit; }
private:
	std::string _serverVer;  //version
	UInt8 _roomNum;         //max room num
	UInt16 _tableNum;        // one room contain max table num
	UInt8 _playerNum;       //one table contain max player num
	UInt8 _readyMode;       //
	UInt8 _gameType;        // game type
	UInt32 _gameMoneyLimit; // game money limit
};

extern ZoGlobal zoGlobal;

#endif