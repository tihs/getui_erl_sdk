#ifndef _I_GT_PUSH_H_
#define _I_GT_PUSH_H_

/***************************版本修改日志***************************
* ==========V1.0.1, chenjb, 2014-07-28==========
* 1. 去除pushMessageToListB接口，pushMessageToListA改名为pushMessageToList
* 2. pushMessageToList增加返回推送详情功能
*
* ==========V1.0.2, chenjb, 2015-01-14==========
* 1. curl bug修改，http://blog.chinaunix.net/uid-20761674-id-3391182.html
*/


#ifdef __cplusplus
extern "C" {
#endif

#if defined(WIN32) || defined(_WIN32)
#	define STDCALL _stdcall
#	if defined(GTPUSHSDK_EXPORTS)
#		define DLL_EXTERN  __declspec(dllexport)
#	else
#		define DLL_EXTERN  __declspec(dllimport)
#	endif
#else
#	define DLL_EXTERN
#	define STDCALL
#endif

// 调用返回结果结构体：SUCCESS(成功)/FAILED(失败)
typedef enum result_t {
    SUCCESS = 0,
    FAILED = 1,
	ERR_ENCODE = 2
} Result;

// 单个结果键值对结构体
typedef struct entry_t {
    char key[100];
    char value[1024];
} Entry;

// 推送结果结构体
typedef struct i_push_result_t {
    int size; // 结果中存在多少个Entry
    Entry entry[10];
} IPushResult;

// 查询结果结构体
typedef struct i_query_result_t {
	char result[255];//成功标志
    int size; // 结果中存在多少个Entry
    Entry data[20];
} IQueryResult;

typedef struct _CustomMsg{
	int size;
	Entry* map;
}CustomMsg;

// 基本消息结构体
typedef struct message_t {
    int isOffline;
    long offlineExpireTime;
    int priority;
	int pushNetWorkType;
} Message;

// 单个消息结构体
typedef struct single_message_t {
    Message msg;
} SingleMessage;

// CID列表消息结构体
typedef struct list_message_t {
    Message msg;
} ListMessage;

// 应用消息结构体
typedef struct app_message_t {
    Message msg;
    char **appIdList;
    int appIdListSize;
    char **phoneTypeList;
    int phoneTypeListSize;
    char **provinceList;
    int provinceListSize;
    char **tagList;
    int tagListSize;
	int speed;
} AppMessage;

// 推送目标结构体
typedef struct target_t {
    char *appId;
    char *clientId;
	char *alias;
} Target;

// 模板类型枚举
typedef enum template_type_t {
    Transmission, PopupTransmission, NotyPopLoad, Notification, Link, APN
} TemplateType;

// 开关枚举
typedef enum _OFF_ON {
    GT_ON = 0, GT_OFF = 1
} off_on;

typedef struct _LISTITEM{
	char item[255];
}ListItem;
typedef struct _LISTINFO{
	int size;
	ListItem* item;
}ListInfo;

// 应用于IOS手机
typedef struct push_info_t {
	int badge;
	char* sound;
	int contentAvailable;
	char* category;
	char* title;
	char* body;
	char* titleLocKey;
	ListInfo titleLocArgs;
	char* actionLocKey;
	char* locKey;
	ListInfo locArgs;
	char* launchImage;
	CustomMsg cmsg;
} PushInfo;

// 基本模板结构体
typedef struct template_t {
    char *appId;
    char *appKey;
    PushInfo pushInfo;
	char* duration_start;
	char* duration_end;
} Template;

// apn推送
typedef struct APNTemplate_template_t{
	Template t;
}APNTemplate;

// 透传模板结构体
typedef struct transmission_template_t {
    Template t;
    int transmissionType;
    char *transmissionContent;
} TransmissionTemplate;

// 弹窗透传模板结构体
typedef struct popup_transmission_template_t {
    Template t;
    int transmissionType;
    char *transmissionContent;
    char *title;
    char *text;
    char *img;
    char *confirmButtonText;
    char *cancelButtonText;
} PopupTransmissionTemplate;

// 通知弹窗下载模板结构体
typedef struct noty_pop_load_template_t {
    Template t;
    char *notyIcon;
    char *logoUrl;
    char *notyTitle;
    char *notyContent;
    off_on isCleared;
    off_on isBelled;
    off_on isVibrationed;
    char *popTitle;
    char *popContent;
    char *popImage;
    char *popButton1;
    char *popButton2;
    char *loadIcon;
    char *loadTitle;
    char *loadUrl;
    int isAutoInstall;
    int isActived;
    char *androidMark;
    char *symbianMark;
    char *iphoneMark;
} NotyPopLoadTemplate;

// 通知模板结构体
typedef struct notification_template_t {
    Template t;
    int transmissionType;
    char *transmissionContent;
    char *text;
    char *title;
    char *logo;
    char *logoUrl;
	off_on isRing;
    off_on isVibrate;
	off_on isClearable;
} NotificationTemplate;

// 链接模板结构体
typedef struct link_template_t {
    Template t;
    char *text;
    char *title;
    char *logo;
    char *logoUrl;
	char *url;
    off_on isRing;
    off_on isVibrate;
	off_on isClearable;
} LinkTemplate;

// 推送结果详情
typedef struct push_detail_t {
	char cid[33]; // 对应的CID
	char ret[51]; // 详情内容
} PushDetail;


// 功能：推送初始化，程序运行前初始化一次即可
// 参数：
//		host 个推服务器URL [in]
//		appKey 个推申请应用的appKey [in]
//		masterSecret 个推申请应用的masterSecret [in]
//		testUTF8 本库所有接口必须传入字符必须为UTF－8编码，这里用于测试是否是UTF－8编码，固定填写"编码"两字 [in]
// 返回：Result枚举, SUCCESS、FAILED、ERR_ENCODE（编码测试失败）
DLL_EXTERN Result STDCALL pushInit(char *host, char *appKey, char *masterSecret, const char *testUTF8);

// 功能：初始化个推服务器鉴权
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
// 返回：Result枚举, SUCCESS、FAILED
DLL_EXTERN Result STDCALL pushConnect(char *appKey);

// 功能：关闭个推服务器鉴权
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
// 返回：Result枚举, SUCCESS、FAILED
DLL_EXTERN Result STDCALL pushClose(char *appKey);

// 功能：推送单条消息
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData 单推消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		target 推送目标结构体指针 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushMessageToSingle(char *appKey, SingleMessage *msgData, void *templateData, TemplateType templateType, Target *target);

// 功能：推送单条消息
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData 单推消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		target 推送目标结构体指针 [in]
//		requestId 用于重试发送 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushMessageToSingleByReqId(char *appKey, SingleMessage *msgData, void *templateData, TemplateType templateType, Target *target, const char *requestId);

// 功能：推送单条消息给ios设备
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData 单推消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		appId 应用识别号 [in]
//		deviceToken ios设备代码 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushAPNMessageToSingle(char *appKey, void *templateData, const char *appId, const char *deviceToken);



// 功能：获取contentId for ios推送
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData CID列表消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		contentId 用于返回contentId的指针 [out]
//		size 可存放contentId的大小 [in]
// 返回：Result枚举, SUCCESS、FAILED
// 注意：如果size小于返回的ID，则返回FAILED
DLL_EXTERN Result STDCALL getAPNContentId(char *appKey, void *templateData, char *contentId, int size);


// 功能：推送消息给tokenlist设备
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		deviceTokenList ios设备代码清单 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushAPNMessageToList(const char *appKey, const char *appId, ListInfo* deviceTokenList, const char* contentId);

// 功能：获取contentId，用于pushMessageToListA接口
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData CID列表消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		contentId 用于返回contentId的指针 [out]
//		size 可存放contentId的大小 [in]
//		groupName 标识组名
// 返回：Result枚举, SUCCESS、FAILED
// 注意：如果size小于返回的ID，则返回FAILED
DLL_EXTERN Result STDCALL getContentIdByGroup(char *appKey, ListMessage *msgData, void *templateData, TemplateType templateType, char *contentId, int size, const char* groupName);

// 功能：获取contentId，用于pushMessageToListA接口
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData CID列表消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		contentId 用于返回contentId的指针 [out]
//		size 可存放contentId的大小 [in]
// 返回：Result枚举, SUCCESS、FAILED
// 注意：如果size小于返回的ID，则返回FAILED
DLL_EXTERN Result STDCALL getContentId(char *appKey, ListMessage *msgData, void *templateData, TemplateType templateType, char *contentId, int size);

// 功能：取消contentId
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		contentId 需要取消的contentId [in]
// 返回：Result枚举, SUCCESS、FAILED
DLL_EXTERN Result STDCALL cancelContentId(char *appKey, char *contentId);

// 功能：推送CID列表
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		contentId 由getContentId返回的contentId [in]
//		targetList 需要推送的目标列表 [in]
//		targetSize 目标列表中有多少个Targe，建议每次50 [in]
//		details 推送详情内容，不需要详情可填写NULL，否则需要预先分配足够内存(至少要targetSize个PushDetail) [in]
// 返回：Result枚举, SUCCESS、FAILED
DLL_EXTERN IPushResult STDCALL pushMessageToList(char *appKey, char *contentId, Target *targetList, int targetSize, PushDetail *details);

// 功能：推送应用消息
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData 应用消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushMessageToApp(char *appKey, AppMessage *msgData, void *templateData, TemplateType templateType);

// 功能：推送应用消息
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		msgData 应用消息结构体指针 [in]
//		templateData 模板结构体指针 [in]
//		templateType 模板类型 [in]
//		groupName	组名 [in]
// 返回：推送结果数据
DLL_EXTERN IPushResult STDCALL pushMessageToAppByGroupName(char *appKey, AppMessage *msgData, void *templateData, TemplateType templateType, const char* groupName);

// 功能：停止推送某个任务
// 参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		contentId 需要停止推送的contentId [in]
// 返回：Result枚举, SUCCESS、FAILED
DLL_EXTERN Result STDCALL pushStop(char *appKey, char *contentId);

// 功能：获取当前SDK版本号
// 参数：无
// 返回：版本号字符串
DLL_EXTERN const char * STDCALL getPushSdkVersion();


//功能：获得当天推送结果
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		date 查询日期 如20150526 [in]
//返回：是否调用成功
DLL_EXTERN IQueryResult STDCALL  queryAppPushDataByDate(const char *appKey, const char* appId, const char* date);

//功能：获得当天用户数据
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		date 查询日期 如20150526 [in]
//返回：是否调用成功
DLL_EXTERN IQueryResult STDCALL  queryAppUserDataByDate(const char *appKey, const char* appId, const char* date);


//功能：根据任务号获得当天数据
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		taskId 任务号 [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  getPushResult(const char *appKey, const char* taskId);


//功能：单个cid绑定别名
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		alias 用户别名 [in]
//		cid 个推用户标识id [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  bindAlias(const char *appKey, const char* appId, const char* alias, const char* cid);

//功能：多个cid绑定别名
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		target 别名与cid对应列表 [in]
//		num	   target列表大小 [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  bindAliasList(const char *appKey, const char* appId, Target *target, int num);


//功能：别名获取cid
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		alias  要查询的别名 [in]
//		list   获得别名绑定的cid列表 [out]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  queryClientId(const char *appKey, const char* appId, const char *alias, char** list);

//功能：释放资源
//参数：
//		list 是queryClientId返回的cid列表
//返回：是否调用成功
DLL_EXTERN Result STDCALL  releaseMem(char* list);

//功能：通过cid获取别名
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		cid	   要查询的cid [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  queryAlias(const char *appKey,const char *appId, const char *cid);

//功能：单个cid和别名解绑
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		alias  要解绑的别名 [in]
//		cid	   要解绑别名的cid [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  unBindAlias(const char *appKey,const char *appId, const char *alias, const char* cid);

//功能：解绑别名所有cid
//参数：
//		appKey 调用pushInit时APP对应的appKey [in]
//		appId 应用识别号 [in]
//		alias 要解绑的别名(解绑对应的所有cid) [in]
//返回：是否调用成功
DLL_EXTERN IPushResult STDCALL  unBindAliasAll(const char *appKey,const char *appId, const char *alias);

#ifdef __cplusplus
}
#endif

#endif
