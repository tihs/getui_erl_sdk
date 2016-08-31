#include "erl_nif.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h> 
#include <time.h>  
#include <IGtPush.h>
#include "equeue.h"
#include <stdbool.h>

ErlNifResourceType* RES_SYNC;
ERL_NIF_TERM atom_ok;
ERL_NIF_TERM atom_error;

#define STACK_STRING_BUFF 255

#define STR_NOT_ENOUGHT_MEMORY "not_enough_memory"
#define STR_DO_FILE_ERROR "do_file_error"

#define DEBUG false

int workers = 7;
static char *host ="http://sdk.open.api.igexin.com/apiex.htm";
char appId[1024] = {0};
char appKey[1024] = {0};
static char *masterSecret = "fk8SdC1h1I81FjNQQh2Ax2";
static char *cid = "";
static char *dt="";

void tosingletest(char *token, char *data);
unsigned long BKDRHash(const char *str);

typedef enum {
    msg_unknown,
    msg_newstate,
    msg_dofile,
    msg_gencall,
    msg_stop
}msg_type;

typedef struct
{
    ErlNifEnv *hold_env;
    msg_type type;

    ErlNifEnv *env;
    ERL_NIF_TERM ref;
    ErlNifPid pid;

    ERL_NIF_TERM arg1;
    ERL_NIF_TERM arg2;
    ERL_NIF_TERM arg3;
    
    char arg_c1[STACK_STRING_BUFF];
    char arg_c2[STACK_STRING_BUFF];

}msg_t;

static msg_t *msg_create() {
    ErlNifEnv *env;
    msg_t *msg;

    env = enif_alloc_env();
    if(env == NULL) {
        return NULL;
    }

    msg = (msg_t *) enif_alloc(sizeof(msg_t));
    if(msg == NULL) {
        enif_free_env(env);
        return NULL;
    }

    msg->env =  env;
    msg->type = msg_unknown;
    msg->ref = 0;

    memset(msg->arg_c1, 0, STACK_STRING_BUFF);
    memset(msg->arg_c2, 0, STACK_STRING_BUFF);

    return msg;
}

void msg_destroy(void *obj)
{
    msg_t *msg = (msg_t *) obj;

    if(msg->env != NULL){
        // fix
        enif_free_env(msg->env);
    }
    enif_free(msg);
}

typedef struct
{
    ErlNifTid tid;
    ErlNifThreadOpts* opts;

    struct queue_t *q;
    int alive;
    int id;
} worker_t;


typedef struct
{
    int count;
    worker_t workers[50];
} Tracker;

Tracker* tracker = NULL;



static ERL_NIF_TERM
make_error_tuple(ErlNifEnv *env, const char *reason)
{
    return enif_make_tuple2(env, atom_error, enif_make_string(env, reason, ERL_NIF_LATIN1));
}

void gencall(ErlNifEnv *env, 
        char *arg1,
        char *arg2);


void exe_msg(msg_t *msg, worker_t *w)
{
    switch(msg->type) {
    case msg_gencall:
        gencall(msg->env,  msg->arg_c1, msg->arg_c2);
    default:
        make_error_tuple(msg->env, "invalid_command");
    }
}

static void *
worker_run(void *arg)
{
    worker_t *w = (worker_t *) arg;
    msg_t *msg;
    int continue_running = 1;

    w->alive = 1;
    while(continue_running) {
        msg = queue_pop(w->q);

        if(msg->type == msg_stop) {
            continue_running = 0;
        }
        else {
            exe_msg(msg, w);
	#ifdef DEBUG
            printf("%d receive\n", w->id);
	#endif
        }
        msg_destroy(msg);
    }

    w->alive = 0;
    return NULL;
}

int worker_init(worker_t *worker,int id)
{
    struct queue_t *q;
    ErlNifThreadOpts* opts;

    q = queue_create();
    if(q == NULL ) {
        goto queue_error;
    }

    worker->q = q;
    worker->id =id;
    opts = enif_thread_opts_create("push_thread_opts");

    if(opts == NULL) {
        goto opts_error;
    }

    worker->opts = opts;
    if(enif_thread_create("push_thread",
                          &worker->tid,
                          worker_run,
                          worker,
                          worker->opts) != 0) {
        goto create_error;
    }

    return 0;

 create_error:
    enif_thread_opts_destroy(opts);
 opts_error:
    queue_destroy(q);
 queue_error:
    return -1;
}

void woker_destory(worker_t *w)
{
    msg_t *msg = msg_create();
    msg->type = msg_stop;
    queue_push(w->q, msg);

    enif_thread_join(w->tid, NULL);
    enif_thread_opts_destroy(w->opts);

    queue_destroy(w->q);
}

int tracker_init(Tracker *t)
{
    int i;
    for(i=0; i< workers; i++) {
        if(worker_init(&t->workers[i], i) <0 ) {
            goto error;
        }
    }

    return 0;

 error:
    while(i>0) {
        --i;
        woker_destory(&t->workers[i]);
    }
    return -1;
}



unsigned int worker_hash(const char *str)
{
    unsigned long idx = BKDRHash(str);
    return idx % workers;
}

static ERL_NIF_TERM
push_command(ErlNifEnv *env,  msg_t *msg)
{
    Tracker *tracker = (Tracker*) enif_priv_data(env);
    int hash_idx;

    hash_idx=worker_hash(msg->arg_c1);

    assert(hash_idx>=0 && hash_idx< workers);
    worker_t *w = &tracker->workers[hash_idx];

    if(!queue_push(w->q, msg)){
        return make_error_tuple(env, "command_push_failed");
    }
#ifdef DEBUG
    printf("%d send push idx:%d\n", w->id, hash_idx);
#endif
    return atom_ok;
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{

    atom_ok = enif_make_atom(env, "ok");
    atom_error = enif_make_atom(env, "error");

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    *priv = (void*) tracker;

    return 0;
}

#define FMT_AND_LIST_NO_MATCH  "fmt and list not match"
#define FMT_AND_RET_NO_MATCH  "fmt and ret not match"
#define FMT_WRONG  "fmt wrong"


void gencall(ErlNifEnv *env, 
        char *arg1,
        char *arg2)
{

    tosingletest(arg1, arg2);

#ifdef DEBUG
    printf("max size:%d  args1:%s args2:%s \r\n", STACK_STRING_BUFF, arg1, arg2);
#endif

}

void printResult(IPushResult result) {
#ifdef DEBUG
    printf("print result:-------------\r\n");
    int i = 0;
    for (i = 0; i < result.size; i++) {
        printf("%s : %s \r\n", result.entry[i].key, result.entry[i].value);
    }
    printf("print end:----------------\r\n");
#endif
}


void TransmissionTemplateDemo(TransmissionTemplate* templ, char *data)
{
    templ->t.appId = appId;
    templ->t.appKey = appKey;
    
    templ->transmissionType = 2;          

    templ->transmissionContent = data;
    //templ->transmissionContent = "{\"sound\":\"test1.wav\",\"badge\":4}";


/*
	templ->t.pushInfo.badge=4;
	templ->t.pushInfo.sound="text1.wav";
	templ->t.pushInfo.contentAvailable=1;
	templ->t.pushInfo.category="";
	Entry cmsg = {0};//
	strcpy(cmsg.key,"");
	strcpy(cmsg.value,"");
	templ->t.pushInfo.cmsg.size=2;
	templ->t.pushInfo.cmsg.map=&cmsg;
	templ->t.pushInfo.body="";
	templ->t.pushInfo.actionLocKey="";
	templ->t.pushInfo.locKey="";
	ListItem locargs[2]={"name","body"};//
	templ->t.pushInfo.locArgs.size=2;
	templ->t.pushInfo.locArgs.item=locargs;
	templ->t.pushInfo.launchImage="";
	
	templ->t.pushInfo.title="";
	templ->t.pushInfo.titleLocKey="";
	ListItem titlelocargs[2]={"",""};//
	templ->t.pushInfo.titleLocArgs.size=2;
	templ->t.pushInfo.titleLocArgs.item=titlelocargs;

*/

} 

void tosingletest(char *token, char *data){
	
    Message msg = {0};
    msg.isOffline = true;
    msg.offlineExpireTime = 1000*3600*2;
    msg.pushNetWorkType = 0;
    SingleMessage singleMsg = {0};
    singleMsg.msg = msg;

	
    Target target = {0};
    //target.clientId = token;
    target.appId = appId;
    
    target.clientId = token;
    IPushResult result = {0};


    TransmissionTemplate tmpl= {0};
    TransmissionTemplateDemo(& tmpl, data);
    //printf("appkey:%s \n", appKey);
    result = pushMessageToSingle(appKey, &singleMsg, &tmpl, Transmission, &target);

    //NotificationTemplate tmpl = {0};
    //NotificationTemplateDemo(& tmpl, data);
    //result = pushMessageToSingle(appKey, &singleMsg, &tmpl, Notification, &target);

    printResult(result);

}

static ERL_NIF_TERM start_getui(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])  
{  

    if (argc != 5)
      return enif_make_badarg(env);


    ErlNifBinary host_bin;
    ErlNifBinary appId_bin;
    ErlNifBinary appKey_bin;
    ErlNifBinary masterSecret_bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &host_bin))
      return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &appId_bin))
      return enif_make_badarg(env);
 
    if (!enif_inspect_iolist_as_binary(env, argv[2], &appKey_bin))
      return enif_make_badarg(env);

    if (!enif_inspect_iolist_as_binary(env, argv[3], &masterSecret_bin))
      return enif_make_badarg(env);

    char host_char[1024] = {0};
    char appId_char[1024] = {0};
    char appKey_char[1024] = {0};
    char masterSecret_char[1024] = {0};
    memcpy(host_char, (char *)host_bin.data, host_bin.size);
    memcpy(appId_char, (char *)appId_bin.data, appId_bin.size);
    memcpy(appId, (char *)appId_bin.data, appId_bin.size);
    memcpy(appKey_char, (char *)appKey_bin.data, appKey_bin.size);
    memcpy(appKey, (char *)appKey_bin.data, appKey_bin.size);

    memcpy(masterSecret_char, (char *)masterSecret_bin.data, masterSecret_bin.size);


    int ret = enif_get_int(env, argv[4], &workers);
    if(!ret || workers <= 0) {
         workers = 8;
    }

    Result r = pushInit(host_char, appKey_char, masterSecret_char, "编码");//"编码"两个字为固定写法，不需要做转换
    if(r!=SUCCESS){
	//printf("pushInit for app failed: ret=%d\n", r);
    }

#ifdef DEBUG
    printf("workers:%d start status:%d \n", workers, r);
#endif

    if(tracker_init(tracker) < 0 ) {
        enif_free(tracker);
        return -1;
    }

    return enif_make_int(env, r);  
    //return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);  
}

unsigned long BKDRHash(const char *str)
{
    unsigned int seed = 131;
    unsigned long hash = 0;

    while (*str)
    {
        hash = hash * seed + (*str++);
    }

    return (hash & 0x7FFFFFFFFFFFFFFF);
}

static ERL_NIF_TERM test(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])  
{  

    if (argc != 1)
      return enif_make_badarg(env);


    ErlNifBinary data_bin;
    if (!enif_inspect_iolist_as_binary(env, argv[0], &data_bin))
      return enif_make_badarg(env);


    char data_char[1024] = {0};
    memcpy(data_char, (char *)data_bin.data, data_bin.size);
    unsigned long hash_value = BKDRHash(data_char);
    printf("hash value:%ld \n", hash_value);

    return enif_make_int(env, 1);  
    //return enif_make_string(env, "Hello world!", ERL_NIF_LATIN1);  
}


static ERL_NIF_TERM
push_message(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    msg_t *msg;

    if(argc != 2) {
        return enif_make_badarg(env);
    }

    if(!enif_is_list(env, argv[0])) {
        return enif_make_badarg(env);
    }

    // fourth arg: list of input args
    if(!enif_is_list(env, argv[1])) {
        return enif_make_badarg(env);
    }

    msg = msg_create();
    if(!msg) {
        return make_error_tuple(env, "command_create_failed");
    }

    if(enif_get_string(env, argv[0], msg->arg_c1, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0){
    	return enif_make_int(env, 0);
    }
    if(enif_get_string(env, argv[1], msg->arg_c2, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0){
    	return enif_make_int(env, 0);
    }

    msg->type = msg_gencall;

    return push_command(env, msg);
}

static ERL_NIF_TERM
gen_bkdr_hash(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

    if(argc != 1) {
        return enif_make_badarg(env);
    }

    char arg_c1[STACK_STRING_BUFF] = {0};
    if(enif_get_string(env, argv[0], arg_c1, STACK_STRING_BUFF, ERL_NIF_LATIN1)<=0){
        return enif_make_int(env, 0);
    }
    unsigned long hash_value = BKDRHash(arg_c1);
    return enif_make_long(env, hash_value);
}

static ErlNifFunc nif_funcs[] = {
    {"start_getui", 5, start_getui}, 
    {"push", 2, push_message}
};

static int reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

ERL_NIF_INIT(getui_erl_sdk, nif_funcs, &load, &reload, NULL, NULL);
