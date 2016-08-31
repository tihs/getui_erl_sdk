#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <IGtPush.h>


static char host[] ="https://api.getui.com/apiex.htm";


static char const *appId = "84oxu8Q1949egtcYV1M1h7";
static char appKey[] = "KME2fV10C798vVhO90kmU1";
static char masterSecret[] = "mJQ0S2qgPl89s1GfGZlt9A";
static char const *cid = "";
static char const *dt="";


void m_pushInit() {
	pushInit(host, appKey, masterSecret, "编码");//"编码"两个字为固定写法，不需要做转换
}

