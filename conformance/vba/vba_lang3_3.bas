Attribute VB_Name = "VbaLang3_3"
Option Explicit

Private Function fV(x As Variant) As Variant
    fV = x
End Function
Private Function Z() As Long
    Z = 0
End Function
Private Sub ByRefL(ByRef n As Long)
    n = n + 1
End Sub
Private Sub ByValL(ByVal n As Long)
    n = n + 1
End Sub
Private Sub ByRefV(ByRef v As Variant)
    v = v & "x"
End Sub
Private Sub ByRefI(ByRef n As Integer)
    n = n + 1
End Sub
Private Function Opt1(Optional a As Variant, Optional b As Long = 7, Optional c As String = "d") As String
    Opt1 = IIf(IsMissing(a), "M", "a=" & a) & "|" & b & "|" & c
End Function
Private Function Opt2(Optional a As Long, Optional b As Variant = Empty) As String
    Opt2 = a & "|" & TypeName(b)
End Function
Private Function PA(ParamArray p() As Variant) As String
    Dim i As Long, s As String
    s = LBound(p) & ":" & UBound(p) & ":"
    For i = LBound(p) To UBound(p)
        s = s & TypeName(p(i)) & ","
    Next i
    PA = s
End Function
Private Function PA2(ByVal a As Long, ParamArray p() As Variant) As String
    PA2 = a & ":" & (UBound(p) - LBound(p) + 1)
End Function
Private Function StatCnt() As Long
    Static n As Long
    n = n + 1
    StatCnt = n
End Function
Private Static Function StatAll() As String
    Dim q As Long
    q = q + 1
    StatAll = CStr(q)
End Function
Private Property Get PG() As Long
    PG = 42
End Property
Private Function Gs(ByVal n As Long) As String
    Dim s As String
    s = "a"
    GoSub Sub1
    s = s & "b"
    If n = 1 Then GoSub Sub2
    Gs = s
    Exit Function
Sub1:
    s = s & "1"
    Return
Sub2:
    s = s & "2"
    Return
End Function
Private Function Dump(v As Variant) As String
    Dim s As String, i As Long
    If IsArray(v) Then
        For i = LBound(v) To UBound(v)
            s = s & Dump(v(i)) & ","
        Next i
        Dump = "[" & s & "]"
    ElseIf IsNull(v) Then
        Dump = "Null"
    ElseIf IsEmpty(v) Then
        Dump = "Empty"
    ElseIf IsObject(v) Then
        Dump = "<" & TypeName(v) & ">"
    Else
        Dump = TypeName(v) & ":" & CStr(v)
    End If
End Function

Public Function LangThree3(ByVal k As Long) As Variant
    Select Case (k - 3000) \ 100
    Case 0: LangThree3 = MC0(k)
    Case 1: LangThree3 = MC1(k)
    Case 2: LangThree3 = MC2(k)
    Case 3: LangThree3 = MC3(k)
    Case 4: LangThree3 = MC4(k)
    Case 5: LangThree3 = MC5(k)
    Case 6: LangThree3 = MC6(k)
    Case 7: LangThree3 = MC7(k)
    Case 8: LangThree3 = MC8(k)
    Case 9: LangThree3 = MC9(k)
    End Select
End Function

Private Function MC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3000: MC0 = M3000()
    Case 3001: MC0 = M3001()
    Case 3002: MC0 = M3002()
    Case 3003: MC0 = M3003()
    Case 3004: MC0 = M3004()
    Case 3005: MC0 = M3005()
    Case 3006: MC0 = M3006()
    Case 3007: MC0 = M3007()
    Case 3008: MC0 = M3008()
    Case 3009: MC0 = M3009()
    Case 3010: MC0 = M3010()
    Case 3011: MC0 = M3011()
    Case 3012: MC0 = M3012()
    Case 3013: MC0 = M3013()
    Case 3014: MC0 = M3014()
    Case 3015: MC0 = M3015()
    Case 3016: MC0 = M3016()
    Case 3017: MC0 = M3017()
    Case 3018: MC0 = M3018()
    Case 3019: MC0 = M3019()
    Case 3020: MC0 = M3020()
    Case 3021: MC0 = M3021()
    Case 3022: MC0 = M3022()
    Case 3023: MC0 = M3023()
    Case 3024: MC0 = M3024()
    Case 3025: MC0 = M3025()
    Case 3026: MC0 = M3026()
    Case 3027: MC0 = M3027()
    Case 3028: MC0 = M3028()
    Case 3029: MC0 = M3029()
    Case 3030: MC0 = M3030()
    Case 3031: MC0 = M3031()
    Case 3032: MC0 = M3032()
    Case 3033: MC0 = M3033()
    Case 3034: MC0 = M3034()
    Case 3035: MC0 = M3035()
    Case 3036: MC0 = M3036()
    Case 3037: MC0 = M3037()
    Case 3038: MC0 = M3038()
    Case 3039: MC0 = M3039()
    Case 3040: MC0 = M3040()
    Case 3041: MC0 = M3041()
    Case 3042: MC0 = M3042()
    Case 3043: MC0 = M3043()
    Case 3044: MC0 = M3044()
    Case 3045: MC0 = M3045()
    Case 3046: MC0 = M3046()
    Case 3047: MC0 = M3047()
    Case 3048: MC0 = M3048()
    Case 3049: MC0 = M3049()
    Case 3050: MC0 = M3050()
    Case 3051: MC0 = M3051()
    Case 3052: MC0 = M3052()
    Case 3053: MC0 = M3053()
    Case 3054: MC0 = M3054()
    Case 3055: MC0 = M3055()
    Case 3056: MC0 = M3056()
    Case 3057: MC0 = M3057()
    Case 3058: MC0 = M3058()
    Case 3059: MC0 = M3059()
    Case 3060: MC0 = M3060()
    Case 3061: MC0 = M3061()
    Case 3062: MC0 = M3062()
    Case 3063: MC0 = M3063()
    Case 3064: MC0 = M3064()
    Case 3065: MC0 = M3065()
    Case 3066: MC0 = M3066()
    Case 3067: MC0 = M3067()
    Case 3068: MC0 = M3068()
    Case 3069: MC0 = M3069()
    Case 3070: MC0 = M3070()
    Case 3071: MC0 = M3071()
    Case 3072: MC0 = M3072()
    Case 3073: MC0 = M3073()
    Case 3074: MC0 = M3074()
    Case 3075: MC0 = M3075()
    Case 3076: MC0 = M3076()
    Case 3077: MC0 = M3077()
    Case 3078: MC0 = M3078()
    Case 3079: MC0 = M3079()
    Case 3080: MC0 = M3080()
    Case 3081: MC0 = M3081()
    Case 3082: MC0 = M3082()
    Case 3083: MC0 = M3083()
    Case 3084: MC0 = M3084()
    Case 3085: MC0 = M3085()
    Case 3086: MC0 = M3086()
    Case 3087: MC0 = M3087()
    Case 3088: MC0 = M3088()
    Case 3089: MC0 = M3089()
    Case 3090: MC0 = M3090()
    Case 3091: MC0 = M3091()
    Case 3092: MC0 = M3092()
    Case 3093: MC0 = M3093()
    Case 3094: MC0 = M3094()
    Case 3095: MC0 = M3095()
    Case 3096: MC0 = M3096()
    Case 3097: MC0 = M3097()
    Case 3098: MC0 = M3098()
    Case 3099: MC0 = M3099()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3100: MC1 = M3100()
    Case 3101: MC1 = M3101()
    Case 3102: MC1 = M3102()
    Case 3103: MC1 = M3103()
    Case 3104: MC1 = M3104()
    Case 3105: MC1 = M3105()
    Case 3106: MC1 = M3106()
    Case 3107: MC1 = M3107()
    Case 3108: MC1 = M3108()
    Case 3109: MC1 = M3109()
    Case 3110: MC1 = M3110()
    Case 3111: MC1 = M3111()
    Case 3112: MC1 = M3112()
    Case 3113: MC1 = M3113()
    Case 3114: MC1 = M3114()
    Case 3115: MC1 = M3115()
    Case 3116: MC1 = M3116()
    Case 3117: MC1 = M3117()
    Case 3118: MC1 = M3118()
    Case 3119: MC1 = M3119()
    Case 3120: MC1 = M3120()
    Case 3121: MC1 = M3121()
    Case 3122: MC1 = M3122()
    Case 3123: MC1 = M3123()
    Case 3124: MC1 = M3124()
    Case 3125: MC1 = M3125()
    Case 3126: MC1 = M3126()
    Case 3127: MC1 = M3127()
    Case 3128: MC1 = M3128()
    Case 3129: MC1 = M3129()
    Case 3130: MC1 = M3130()
    Case 3131: MC1 = M3131()
    Case 3132: MC1 = M3132()
    Case 3133: MC1 = M3133()
    Case 3134: MC1 = M3134()
    Case 3135: MC1 = M3135()
    Case 3136: MC1 = M3136()
    Case 3137: MC1 = M3137()
    Case 3138: MC1 = M3138()
    Case 3139: MC1 = M3139()
    Case 3140: MC1 = M3140()
    Case 3141: MC1 = M3141()
    Case 3142: MC1 = M3142()
    Case 3143: MC1 = M3143()
    Case 3144: MC1 = M3144()
    Case 3145: MC1 = M3145()
    Case 3146: MC1 = M3146()
    Case 3147: MC1 = M3147()
    Case 3148: MC1 = M3148()
    Case 3149: MC1 = M3149()
    Case 3150: MC1 = M3150()
    Case 3151: MC1 = M3151()
    Case 3152: MC1 = M3152()
    Case 3153: MC1 = M3153()
    Case 3154: MC1 = M3154()
    Case 3155: MC1 = M3155()
    Case 3156: MC1 = M3156()
    Case 3157: MC1 = M3157()
    Case 3158: MC1 = M3158()
    Case 3159: MC1 = M3159()
    Case 3160: MC1 = M3160()
    Case 3161: MC1 = M3161()
    Case 3162: MC1 = M3162()
    Case 3163: MC1 = M3163()
    Case 3164: MC1 = M3164()
    Case 3165: MC1 = M3165()
    Case 3166: MC1 = M3166()
    Case 3167: MC1 = M3167()
    Case 3168: MC1 = M3168()
    Case 3169: MC1 = M3169()
    Case 3170: MC1 = M3170()
    Case 3171: MC1 = M3171()
    Case 3172: MC1 = M3172()
    Case 3173: MC1 = M3173()
    Case 3174: MC1 = M3174()
    Case 3175: MC1 = M3175()
    Case 3176: MC1 = M3176()
    Case 3177: MC1 = M3177()
    Case 3178: MC1 = M3178()
    Case 3179: MC1 = M3179()
    Case 3180: MC1 = M3180()
    Case 3181: MC1 = M3181()
    Case 3182: MC1 = M3182()
    Case 3183: MC1 = M3183()
    Case 3184: MC1 = M3184()
    Case 3185: MC1 = M3185()
    Case 3186: MC1 = M3186()
    Case 3187: MC1 = M3187()
    Case 3188: MC1 = M3188()
    Case 3189: MC1 = M3189()
    Case 3190: MC1 = M3190()
    Case 3191: MC1 = M3191()
    Case 3192: MC1 = M3192()
    Case 3193: MC1 = M3193()
    Case 3194: MC1 = M3194()
    Case 3195: MC1 = M3195()
    Case 3196: MC1 = M3196()
    Case 3197: MC1 = M3197()
    Case 3198: MC1 = M3198()
    Case 3199: MC1 = M3199()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3200: MC2 = M3200()
    Case 3201: MC2 = M3201()
    Case 3202: MC2 = M3202()
    Case 3203: MC2 = M3203()
    Case 3204: MC2 = M3204()
    Case 3205: MC2 = M3205()
    Case 3206: MC2 = M3206()
    Case 3207: MC2 = M3207()
    Case 3208: MC2 = M3208()
    Case 3209: MC2 = M3209()
    Case 3210: MC2 = M3210()
    Case 3211: MC2 = M3211()
    Case 3212: MC2 = M3212()
    Case 3213: MC2 = M3213()
    Case 3214: MC2 = M3214()
    Case 3215: MC2 = M3215()
    Case 3216: MC2 = M3216()
    Case 3217: MC2 = M3217()
    Case 3218: MC2 = M3218()
    Case 3219: MC2 = M3219()
    Case 3220: MC2 = M3220()
    Case 3221: MC2 = M3221()
    Case 3222: MC2 = M3222()
    Case 3223: MC2 = M3223()
    Case 3224: MC2 = M3224()
    Case 3225: MC2 = M3225()
    Case 3226: MC2 = M3226()
    Case 3227: MC2 = M3227()
    Case 3228: MC2 = M3228()
    Case 3229: MC2 = M3229()
    Case 3230: MC2 = M3230()
    Case 3231: MC2 = M3231()
    Case 3232: MC2 = M3232()
    Case 3233: MC2 = M3233()
    Case 3234: MC2 = M3234()
    Case 3235: MC2 = M3235()
    Case 3236: MC2 = M3236()
    Case 3237: MC2 = M3237()
    Case 3238: MC2 = M3238()
    Case 3239: MC2 = M3239()
    Case 3240: MC2 = M3240()
    Case 3241: MC2 = M3241()
    Case 3242: MC2 = M3242()
    Case 3243: MC2 = M3243()
    Case 3244: MC2 = M3244()
    Case 3245: MC2 = M3245()
    Case 3246: MC2 = M3246()
    Case 3247: MC2 = M3247()
    Case 3248: MC2 = M3248()
    Case 3249: MC2 = M3249()
    Case 3250: MC2 = M3250()
    Case 3251: MC2 = M3251()
    Case 3252: MC2 = M3252()
    Case 3253: MC2 = M3253()
    Case 3254: MC2 = M3254()
    Case 3255: MC2 = M3255()
    Case 3256: MC2 = M3256()
    Case 3257: MC2 = M3257()
    Case 3258: MC2 = M3258()
    Case 3259: MC2 = M3259()
    Case 3260: MC2 = M3260()
    Case 3261: MC2 = M3261()
    Case 3262: MC2 = M3262()
    Case 3263: MC2 = M3263()
    Case 3264: MC2 = M3264()
    Case 3265: MC2 = M3265()
    Case 3266: MC2 = M3266()
    Case 3267: MC2 = M3267()
    Case 3268: MC2 = M3268()
    Case 3269: MC2 = M3269()
    Case 3270: MC2 = M3270()
    Case 3271: MC2 = M3271()
    Case 3272: MC2 = M3272()
    Case 3273: MC2 = M3273()
    Case 3274: MC2 = M3274()
    Case 3275: MC2 = M3275()
    Case 3276: MC2 = M3276()
    Case 3277: MC2 = M3277()
    Case 3278: MC2 = M3278()
    Case 3279: MC2 = M3279()
    Case 3280: MC2 = M3280()
    Case 3281: MC2 = M3281()
    Case 3282: MC2 = M3282()
    Case 3283: MC2 = M3283()
    Case 3284: MC2 = M3284()
    Case 3285: MC2 = M3285()
    Case 3286: MC2 = M3286()
    Case 3287: MC2 = M3287()
    Case 3288: MC2 = M3288()
    Case 3289: MC2 = M3289()
    Case 3290: MC2 = M3290()
    Case 3291: MC2 = M3291()
    Case 3292: MC2 = M3292()
    Case 3293: MC2 = M3293()
    Case 3294: MC2 = M3294()
    Case 3295: MC2 = M3295()
    Case 3296: MC2 = M3296()
    Case 3297: MC2 = M3297()
    Case 3298: MC2 = M3298()
    Case 3299: MC2 = M3299()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3300: MC3 = M3300()
    Case 3301: MC3 = M3301()
    Case 3302: MC3 = M3302()
    Case 3303: MC3 = M3303()
    Case 3304: MC3 = M3304()
    Case 3305: MC3 = M3305()
    Case 3306: MC3 = M3306()
    Case 3307: MC3 = M3307()
    Case 3308: MC3 = M3308()
    Case 3309: MC3 = M3309()
    Case 3310: MC3 = M3310()
    Case 3311: MC3 = M3311()
    Case 3312: MC3 = M3312()
    Case 3313: MC3 = M3313()
    Case 3314: MC3 = M3314()
    Case 3315: MC3 = M3315()
    Case 3316: MC3 = M3316()
    Case 3317: MC3 = M3317()
    Case 3318: MC3 = M3318()
    Case 3319: MC3 = M3319()
    Case 3320: MC3 = M3320()
    Case 3321: MC3 = M3321()
    Case 3322: MC3 = M3322()
    Case 3323: MC3 = M3323()
    Case 3324: MC3 = M3324()
    Case 3325: MC3 = M3325()
    Case 3326: MC3 = M3326()
    Case 3327: MC3 = M3327()
    Case 3328: MC3 = M3328()
    Case 3329: MC3 = M3329()
    Case 3330: MC3 = M3330()
    Case 3331: MC3 = M3331()
    Case 3332: MC3 = M3332()
    Case 3333: MC3 = M3333()
    Case 3334: MC3 = M3334()
    Case 3335: MC3 = M3335()
    Case 3336: MC3 = M3336()
    Case 3337: MC3 = M3337()
    Case 3338: MC3 = M3338()
    Case 3339: MC3 = M3339()
    Case 3340: MC3 = M3340()
    Case 3341: MC3 = M3341()
    Case 3342: MC3 = M3342()
    Case 3343: MC3 = M3343()
    Case 3344: MC3 = M3344()
    Case 3345: MC3 = M3345()
    Case 3346: MC3 = M3346()
    Case 3347: MC3 = M3347()
    Case 3348: MC3 = M3348()
    Case 3349: MC3 = M3349()
    Case 3350: MC3 = M3350()
    Case 3351: MC3 = M3351()
    Case 3352: MC3 = M3352()
    Case 3353: MC3 = M3353()
    Case 3354: MC3 = M3354()
    Case 3355: MC3 = M3355()
    Case 3356: MC3 = M3356()
    Case 3357: MC3 = M3357()
    Case 3358: MC3 = M3358()
    Case 3359: MC3 = M3359()
    Case 3360: MC3 = M3360()
    Case 3361: MC3 = M3361()
    Case 3362: MC3 = M3362()
    Case 3363: MC3 = M3363()
    Case 3364: MC3 = M3364()
    Case 3365: MC3 = M3365()
    Case 3366: MC3 = M3366()
    Case 3367: MC3 = M3367()
    Case 3368: MC3 = M3368()
    Case 3369: MC3 = M3369()
    Case 3370: MC3 = M3370()
    Case 3371: MC3 = M3371()
    Case 3372: MC3 = M3372()
    Case 3373: MC3 = M3373()
    Case 3374: MC3 = M3374()
    Case 3375: MC3 = M3375()
    Case 3376: MC3 = M3376()
    Case 3377: MC3 = M3377()
    Case 3378: MC3 = M3378()
    Case 3379: MC3 = M3379()
    Case 3380: MC3 = M3380()
    Case 3381: MC3 = M3381()
    Case 3382: MC3 = M3382()
    Case 3383: MC3 = M3383()
    Case 3384: MC3 = M3384()
    Case 3385: MC3 = M3385()
    Case 3386: MC3 = M3386()
    Case 3387: MC3 = M3387()
    Case 3388: MC3 = M3388()
    Case 3389: MC3 = M3389()
    Case 3390: MC3 = M3390()
    Case 3391: MC3 = M3391()
    Case 3392: MC3 = M3392()
    Case 3393: MC3 = M3393()
    Case 3394: MC3 = M3394()
    Case 3395: MC3 = M3395()
    Case 3396: MC3 = M3396()
    Case 3397: MC3 = M3397()
    Case 3398: MC3 = M3398()
    Case 3399: MC3 = M3399()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3400: MC4 = M3400()
    Case 3401: MC4 = M3401()
    Case 3402: MC4 = M3402()
    Case 3403: MC4 = M3403()
    Case 3404: MC4 = M3404()
    Case 3405: MC4 = M3405()
    Case 3406: MC4 = M3406()
    Case 3407: MC4 = M3407()
    Case 3408: MC4 = M3408()
    Case 3409: MC4 = M3409()
    Case 3410: MC4 = M3410()
    Case 3411: MC4 = M3411()
    Case 3412: MC4 = M3412()
    Case 3413: MC4 = M3413()
    Case 3414: MC4 = M3414()
    Case 3415: MC4 = M3415()
    Case 3416: MC4 = M3416()
    Case 3417: MC4 = M3417()
    Case 3418: MC4 = M3418()
    Case 3419: MC4 = M3419()
    Case 3420: MC4 = M3420()
    Case 3421: MC4 = M3421()
    Case 3422: MC4 = M3422()
    Case 3423: MC4 = M3423()
    Case 3424: MC4 = M3424()
    Case 3425: MC4 = M3425()
    Case 3426: MC4 = M3426()
    Case 3427: MC4 = M3427()
    Case 3428: MC4 = M3428()
    Case 3429: MC4 = M3429()
    Case 3430: MC4 = M3430()
    Case 3431: MC4 = M3431()
    Case 3432: MC4 = M3432()
    Case 3433: MC4 = M3433()
    Case 3434: MC4 = M3434()
    Case 3435: MC4 = M3435()
    Case 3436: MC4 = M3436()
    Case 3437: MC4 = M3437()
    Case 3438: MC4 = M3438()
    Case 3439: MC4 = M3439()
    Case 3440: MC4 = M3440()
    Case 3441: MC4 = M3441()
    Case 3442: MC4 = M3442()
    Case 3443: MC4 = M3443()
    Case 3444: MC4 = M3444()
    Case 3445: MC4 = M3445()
    Case 3446: MC4 = M3446()
    Case 3447: MC4 = M3447()
    Case 3448: MC4 = M3448()
    Case 3449: MC4 = M3449()
    Case 3450: MC4 = M3450()
    Case 3451: MC4 = M3451()
    Case 3452: MC4 = M3452()
    Case 3453: MC4 = M3453()
    Case 3454: MC4 = M3454()
    Case 3455: MC4 = M3455()
    Case 3456: MC4 = M3456()
    Case 3457: MC4 = M3457()
    Case 3458: MC4 = M3458()
    Case 3459: MC4 = M3459()
    Case 3460: MC4 = M3460()
    Case 3461: MC4 = M3461()
    Case 3462: MC4 = M3462()
    Case 3463: MC4 = M3463()
    Case 3464: MC4 = M3464()
    Case 3465: MC4 = M3465()
    Case 3466: MC4 = M3466()
    Case 3467: MC4 = M3467()
    Case 3468: MC4 = M3468()
    Case 3469: MC4 = M3469()
    Case 3470: MC4 = M3470()
    Case 3471: MC4 = M3471()
    Case 3472: MC4 = M3472()
    Case 3473: MC4 = M3473()
    Case 3474: MC4 = M3474()
    Case 3475: MC4 = M3475()
    Case 3476: MC4 = M3476()
    Case 3477: MC4 = M3477()
    Case 3478: MC4 = M3478()
    Case 3479: MC4 = M3479()
    Case 3480: MC4 = M3480()
    Case 3481: MC4 = M3481()
    Case 3482: MC4 = M3482()
    Case 3483: MC4 = M3483()
    Case 3484: MC4 = M3484()
    Case 3485: MC4 = M3485()
    Case 3486: MC4 = M3486()
    Case 3487: MC4 = M3487()
    Case 3488: MC4 = M3488()
    Case 3489: MC4 = M3489()
    Case 3490: MC4 = M3490()
    Case 3491: MC4 = M3491()
    Case 3492: MC4 = M3492()
    Case 3493: MC4 = M3493()
    Case 3494: MC4 = M3494()
    Case 3495: MC4 = M3495()
    Case 3496: MC4 = M3496()
    Case 3497: MC4 = M3497()
    Case 3498: MC4 = M3498()
    Case 3499: MC4 = M3499()
    End Select
    Exit Function
EH:
    MC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3500: MC5 = M3500()
    Case 3501: MC5 = M3501()
    Case 3502: MC5 = M3502()
    Case 3503: MC5 = M3503()
    Case 3504: MC5 = M3504()
    Case 3505: MC5 = M3505()
    Case 3506: MC5 = M3506()
    Case 3507: MC5 = M3507()
    Case 3508: MC5 = M3508()
    Case 3509: MC5 = M3509()
    Case 3510: MC5 = M3510()
    Case 3511: MC5 = M3511()
    Case 3512: MC5 = M3512()
    Case 3513: MC5 = M3513()
    Case 3514: MC5 = M3514()
    Case 3515: MC5 = M3515()
    Case 3516: MC5 = M3516()
    Case 3517: MC5 = M3517()
    Case 3518: MC5 = M3518()
    Case 3519: MC5 = M3519()
    Case 3520: MC5 = M3520()
    Case 3521: MC5 = M3521()
    Case 3522: MC5 = M3522()
    Case 3523: MC5 = M3523()
    Case 3524: MC5 = M3524()
    Case 3525: MC5 = M3525()
    Case 3526: MC5 = M3526()
    Case 3527: MC5 = M3527()
    Case 3528: MC5 = M3528()
    Case 3529: MC5 = M3529()
    Case 3530: MC5 = M3530()
    Case 3531: MC5 = M3531()
    Case 3532: MC5 = M3532()
    Case 3533: MC5 = M3533()
    Case 3534: MC5 = M3534()
    Case 3535: MC5 = M3535()
    Case 3536: MC5 = M3536()
    Case 3537: MC5 = M3537()
    Case 3538: MC5 = M3538()
    Case 3539: MC5 = M3539()
    Case 3540: MC5 = M3540()
    Case 3541: MC5 = M3541()
    Case 3542: MC5 = M3542()
    Case 3543: MC5 = M3543()
    Case 3544: MC5 = M3544()
    Case 3545: MC5 = M3545()
    Case 3546: MC5 = M3546()
    Case 3547: MC5 = M3547()
    Case 3548: MC5 = M3548()
    Case 3549: MC5 = M3549()
    Case 3550: MC5 = M3550()
    Case 3551: MC5 = M3551()
    Case 3552: MC5 = M3552()
    Case 3553: MC5 = M3553()
    Case 3554: MC5 = M3554()
    Case 3555: MC5 = M3555()
    Case 3556: MC5 = M3556()
    Case 3557: MC5 = M3557()
    Case 3558: MC5 = M3558()
    Case 3559: MC5 = M3559()
    Case 3560: MC5 = M3560()
    Case 3561: MC5 = M3561()
    Case 3562: MC5 = M3562()
    Case 3563: MC5 = M3563()
    Case 3564: MC5 = M3564()
    Case 3565: MC5 = M3565()
    Case 3566: MC5 = M3566()
    Case 3567: MC5 = M3567()
    Case 3568: MC5 = M3568()
    Case 3569: MC5 = M3569()
    Case 3570: MC5 = M3570()
    Case 3571: MC5 = M3571()
    Case 3572: MC5 = M3572()
    Case 3573: MC5 = M3573()
    Case 3574: MC5 = M3574()
    Case 3575: MC5 = M3575()
    Case 3576: MC5 = M3576()
    Case 3577: MC5 = M3577()
    Case 3578: MC5 = M3578()
    Case 3579: MC5 = M3579()
    Case 3580: MC5 = M3580()
    Case 3581: MC5 = M3581()
    Case 3582: MC5 = M3582()
    Case 3583: MC5 = M3583()
    Case 3584: MC5 = M3584()
    Case 3585: MC5 = M3585()
    Case 3586: MC5 = M3586()
    Case 3587: MC5 = M3587()
    Case 3588: MC5 = M3588()
    Case 3589: MC5 = M3589()
    Case 3590: MC5 = M3590()
    Case 3591: MC5 = M3591()
    Case 3592: MC5 = M3592()
    Case 3593: MC5 = M3593()
    Case 3594: MC5 = M3594()
    Case 3595: MC5 = M3595()
    Case 3596: MC5 = M3596()
    Case 3597: MC5 = M3597()
    Case 3598: MC5 = M3598()
    Case 3599: MC5 = M3599()
    End Select
    Exit Function
EH:
    MC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3600: MC6 = M3600()
    Case 3601: MC6 = M3601()
    Case 3602: MC6 = M3602()
    Case 3603: MC6 = M3603()
    Case 3604: MC6 = M3604()
    Case 3605: MC6 = M3605()
    Case 3606: MC6 = M3606()
    Case 3607: MC6 = M3607()
    Case 3608: MC6 = M3608()
    Case 3609: MC6 = M3609()
    Case 3610: MC6 = M3610()
    Case 3611: MC6 = M3611()
    Case 3612: MC6 = M3612()
    Case 3613: MC6 = M3613()
    Case 3614: MC6 = M3614()
    Case 3615: MC6 = M3615()
    Case 3616: MC6 = M3616()
    Case 3617: MC6 = M3617()
    Case 3618: MC6 = M3618()
    Case 3619: MC6 = M3619()
    Case 3620: MC6 = M3620()
    Case 3621: MC6 = M3621()
    Case 3622: MC6 = M3622()
    Case 3623: MC6 = M3623()
    Case 3624: MC6 = M3624()
    Case 3625: MC6 = M3625()
    Case 3626: MC6 = M3626()
    Case 3627: MC6 = M3627()
    Case 3628: MC6 = M3628()
    Case 3629: MC6 = M3629()
    Case 3630: MC6 = M3630()
    Case 3631: MC6 = M3631()
    Case 3632: MC6 = M3632()
    Case 3633: MC6 = M3633()
    Case 3634: MC6 = M3634()
    Case 3635: MC6 = M3635()
    Case 3636: MC6 = M3636()
    Case 3637: MC6 = M3637()
    Case 3638: MC6 = M3638()
    Case 3639: MC6 = M3639()
    Case 3640: MC6 = M3640()
    Case 3641: MC6 = M3641()
    Case 3642: MC6 = M3642()
    Case 3643: MC6 = M3643()
    Case 3644: MC6 = M3644()
    Case 3645: MC6 = M3645()
    Case 3646: MC6 = M3646()
    Case 3647: MC6 = M3647()
    Case 3648: MC6 = M3648()
    Case 3649: MC6 = M3649()
    Case 3650: MC6 = M3650()
    Case 3651: MC6 = M3651()
    Case 3652: MC6 = M3652()
    Case 3653: MC6 = M3653()
    Case 3654: MC6 = M3654()
    Case 3655: MC6 = M3655()
    Case 3656: MC6 = M3656()
    Case 3657: MC6 = M3657()
    Case 3658: MC6 = M3658()
    Case 3659: MC6 = M3659()
    Case 3660: MC6 = M3660()
    Case 3661: MC6 = M3661()
    Case 3662: MC6 = M3662()
    Case 3663: MC6 = M3663()
    Case 3664: MC6 = M3664()
    Case 3665: MC6 = M3665()
    Case 3666: MC6 = M3666()
    Case 3667: MC6 = M3667()
    Case 3668: MC6 = M3668()
    Case 3669: MC6 = M3669()
    Case 3670: MC6 = M3670()
    Case 3671: MC6 = M3671()
    Case 3672: MC6 = M3672()
    Case 3673: MC6 = M3673()
    Case 3674: MC6 = M3674()
    Case 3675: MC6 = M3675()
    Case 3676: MC6 = M3676()
    Case 3677: MC6 = M3677()
    Case 3678: MC6 = M3678()
    Case 3679: MC6 = M3679()
    Case 3680: MC6 = M3680()
    Case 3681: MC6 = M3681()
    Case 3682: MC6 = M3682()
    Case 3683: MC6 = M3683()
    Case 3684: MC6 = M3684()
    Case 3685: MC6 = M3685()
    Case 3686: MC6 = M3686()
    Case 3687: MC6 = M3687()
    Case 3688: MC6 = M3688()
    Case 3689: MC6 = M3689()
    Case 3690: MC6 = M3690()
    Case 3691: MC6 = M3691()
    Case 3692: MC6 = M3692()
    Case 3693: MC6 = M3693()
    Case 3694: MC6 = M3694()
    Case 3695: MC6 = M3695()
    Case 3696: MC6 = M3696()
    Case 3697: MC6 = M3697()
    Case 3698: MC6 = M3698()
    Case 3699: MC6 = M3699()
    End Select
    Exit Function
EH:
    MC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3700: MC7 = M3700()
    Case 3701: MC7 = M3701()
    Case 3702: MC7 = M3702()
    Case 3703: MC7 = M3703()
    Case 3704: MC7 = M3704()
    Case 3705: MC7 = M3705()
    Case 3706: MC7 = M3706()
    Case 3707: MC7 = M3707()
    Case 3708: MC7 = M3708()
    Case 3709: MC7 = M3709()
    Case 3710: MC7 = M3710()
    Case 3711: MC7 = M3711()
    Case 3712: MC7 = M3712()
    Case 3713: MC7 = M3713()
    Case 3714: MC7 = M3714()
    Case 3715: MC7 = M3715()
    Case 3716: MC7 = M3716()
    Case 3717: MC7 = M3717()
    Case 3718: MC7 = M3718()
    Case 3719: MC7 = M3719()
    Case 3720: MC7 = M3720()
    Case 3721: MC7 = M3721()
    Case 3722: MC7 = M3722()
    Case 3723: MC7 = M3723()
    Case 3724: MC7 = M3724()
    Case 3725: MC7 = M3725()
    Case 3726: MC7 = M3726()
    Case 3727: MC7 = M3727()
    Case 3728: MC7 = M3728()
    Case 3729: MC7 = M3729()
    Case 3730: MC7 = M3730()
    Case 3731: MC7 = M3731()
    Case 3732: MC7 = M3732()
    Case 3733: MC7 = M3733()
    Case 3734: MC7 = M3734()
    Case 3735: MC7 = M3735()
    Case 3736: MC7 = M3736()
    Case 3737: MC7 = M3737()
    Case 3738: MC7 = M3738()
    Case 3739: MC7 = M3739()
    Case 3740: MC7 = M3740()
    Case 3741: MC7 = M3741()
    Case 3742: MC7 = M3742()
    Case 3743: MC7 = M3743()
    Case 3744: MC7 = M3744()
    Case 3745: MC7 = M3745()
    Case 3746: MC7 = M3746()
    Case 3747: MC7 = M3747()
    Case 3748: MC7 = M3748()
    Case 3749: MC7 = M3749()
    Case 3750: MC7 = M3750()
    Case 3751: MC7 = M3751()
    Case 3752: MC7 = M3752()
    Case 3753: MC7 = M3753()
    Case 3754: MC7 = M3754()
    Case 3755: MC7 = M3755()
    Case 3756: MC7 = M3756()
    Case 3757: MC7 = M3757()
    Case 3758: MC7 = M3758()
    Case 3759: MC7 = M3759()
    Case 3760: MC7 = M3760()
    Case 3761: MC7 = M3761()
    Case 3762: MC7 = M3762()
    Case 3763: MC7 = M3763()
    Case 3764: MC7 = M3764()
    Case 3765: MC7 = M3765()
    Case 3766: MC7 = M3766()
    Case 3767: MC7 = M3767()
    Case 3768: MC7 = M3768()
    Case 3769: MC7 = M3769()
    Case 3770: MC7 = M3770()
    Case 3771: MC7 = M3771()
    Case 3772: MC7 = M3772()
    Case 3773: MC7 = M3773()
    Case 3774: MC7 = M3774()
    Case 3775: MC7 = M3775()
    Case 3776: MC7 = M3776()
    Case 3777: MC7 = M3777()
    Case 3778: MC7 = M3778()
    Case 3779: MC7 = M3779()
    Case 3780: MC7 = M3780()
    Case 3781: MC7 = M3781()
    Case 3782: MC7 = M3782()
    Case 3783: MC7 = M3783()
    Case 3784: MC7 = M3784()
    Case 3785: MC7 = M3785()
    Case 3786: MC7 = M3786()
    Case 3787: MC7 = M3787()
    Case 3788: MC7 = M3788()
    Case 3789: MC7 = M3789()
    Case 3790: MC7 = M3790()
    Case 3791: MC7 = M3791()
    Case 3792: MC7 = M3792()
    Case 3793: MC7 = M3793()
    Case 3794: MC7 = M3794()
    Case 3795: MC7 = M3795()
    Case 3796: MC7 = M3796()
    Case 3797: MC7 = M3797()
    Case 3798: MC7 = M3798()
    Case 3799: MC7 = M3799()
    End Select
    Exit Function
EH:
    MC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3800: MC8 = M3800()
    Case 3801: MC8 = M3801()
    Case 3802: MC8 = M3802()
    Case 3803: MC8 = M3803()
    Case 3804: MC8 = M3804()
    Case 3805: MC8 = M3805()
    Case 3806: MC8 = M3806()
    Case 3807: MC8 = M3807()
    Case 3808: MC8 = M3808()
    Case 3809: MC8 = M3809()
    Case 3810: MC8 = M3810()
    Case 3811: MC8 = M3811()
    Case 3812: MC8 = M3812()
    Case 3813: MC8 = M3813()
    Case 3814: MC8 = M3814()
    Case 3815: MC8 = M3815()
    Case 3816: MC8 = M3816()
    Case 3817: MC8 = M3817()
    Case 3818: MC8 = M3818()
    Case 3819: MC8 = M3819()
    Case 3820: MC8 = M3820()
    Case 3821: MC8 = M3821()
    Case 3822: MC8 = M3822()
    Case 3823: MC8 = M3823()
    Case 3824: MC8 = M3824()
    Case 3825: MC8 = M3825()
    Case 3826: MC8 = M3826()
    Case 3827: MC8 = M3827()
    Case 3828: MC8 = M3828()
    Case 3829: MC8 = M3829()
    Case 3830: MC8 = M3830()
    Case 3831: MC8 = M3831()
    Case 3832: MC8 = M3832()
    Case 3833: MC8 = M3833()
    Case 3834: MC8 = M3834()
    Case 3835: MC8 = M3835()
    Case 3836: MC8 = M3836()
    Case 3837: MC8 = M3837()
    Case 3838: MC8 = M3838()
    Case 3839: MC8 = M3839()
    Case 3840: MC8 = M3840()
    Case 3841: MC8 = M3841()
    Case 3842: MC8 = M3842()
    Case 3843: MC8 = M3843()
    Case 3844: MC8 = M3844()
    Case 3845: MC8 = M3845()
    Case 3846: MC8 = M3846()
    Case 3847: MC8 = M3847()
    Case 3848: MC8 = M3848()
    Case 3849: MC8 = M3849()
    Case 3850: MC8 = M3850()
    Case 3851: MC8 = M3851()
    Case 3852: MC8 = M3852()
    Case 3853: MC8 = M3853()
    Case 3854: MC8 = M3854()
    Case 3855: MC8 = M3855()
    Case 3856: MC8 = M3856()
    Case 3857: MC8 = M3857()
    Case 3858: MC8 = M3858()
    Case 3859: MC8 = M3859()
    Case 3860: MC8 = M3860()
    Case 3861: MC8 = M3861()
    Case 3862: MC8 = M3862()
    Case 3863: MC8 = M3863()
    Case 3864: MC8 = M3864()
    Case 3865: MC8 = M3865()
    Case 3866: MC8 = M3866()
    Case 3867: MC8 = M3867()
    Case 3868: MC8 = M3868()
    Case 3869: MC8 = M3869()
    Case 3870: MC8 = M3870()
    Case 3871: MC8 = M3871()
    Case 3872: MC8 = M3872()
    Case 3873: MC8 = M3873()
    Case 3874: MC8 = M3874()
    Case 3875: MC8 = M3875()
    Case 3876: MC8 = M3876()
    Case 3877: MC8 = M3877()
    Case 3878: MC8 = M3878()
    Case 3879: MC8 = M3879()
    Case 3880: MC8 = M3880()
    Case 3881: MC8 = M3881()
    Case 3882: MC8 = M3882()
    Case 3883: MC8 = M3883()
    Case 3884: MC8 = M3884()
    Case 3885: MC8 = M3885()
    Case 3886: MC8 = M3886()
    Case 3887: MC8 = M3887()
    Case 3888: MC8 = M3888()
    Case 3889: MC8 = M3889()
    Case 3890: MC8 = M3890()
    Case 3891: MC8 = M3891()
    Case 3892: MC8 = M3892()
    Case 3893: MC8 = M3893()
    Case 3894: MC8 = M3894()
    Case 3895: MC8 = M3895()
    Case 3896: MC8 = M3896()
    Case 3897: MC8 = M3897()
    Case 3898: MC8 = M3898()
    Case 3899: MC8 = M3899()
    End Select
    Exit Function
EH:
    MC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 3900: MC9 = M3900()
    Case 3901: MC9 = M3901()
    Case 3902: MC9 = M3902()
    Case 3903: MC9 = M3903()
    Case 3904: MC9 = M3904()
    Case 3905: MC9 = M3905()
    Case 3906: MC9 = M3906()
    Case 3907: MC9 = M3907()
    Case 3908: MC9 = M3908()
    Case 3909: MC9 = M3909()
    Case 3910: MC9 = M3910()
    Case 3911: MC9 = M3911()
    Case 3912: MC9 = M3912()
    Case 3913: MC9 = M3913()
    Case 3914: MC9 = M3914()
    Case 3915: MC9 = M3915()
    Case 3916: MC9 = M3916()
    Case 3917: MC9 = M3917()
    Case 3918: MC9 = M3918()
    Case 3919: MC9 = M3919()
    Case 3920: MC9 = M3920()
    Case 3921: MC9 = M3921()
    Case 3922: MC9 = M3922()
    Case 3923: MC9 = M3923()
    Case 3924: MC9 = M3924()
    Case 3925: MC9 = M3925()
    Case 3926: MC9 = M3926()
    Case 3927: MC9 = M3927()
    Case 3928: MC9 = M3928()
    Case 3929: MC9 = M3929()
    Case 3930: MC9 = M3930()
    Case 3931: MC9 = M3931()
    Case 3932: MC9 = M3932()
    Case 3933: MC9 = M3933()
    Case 3934: MC9 = M3934()
    Case 3935: MC9 = M3935()
    Case 3936: MC9 = M3936()
    Case 3937: MC9 = M3937()
    Case 3938: MC9 = M3938()
    Case 3939: MC9 = M3939()
    Case 3940: MC9 = M3940()
    Case 3941: MC9 = M3941()
    Case 3942: MC9 = M3942()
    Case 3943: MC9 = M3943()
    Case 3944: MC9 = M3944()
    Case 3945: MC9 = M3945()
    Case 3946: MC9 = M3946()
    Case 3947: MC9 = M3947()
    Case 3948: MC9 = M3948()
    Case 3949: MC9 = M3949()
    Case 3950: MC9 = M3950()
    Case 3951: MC9 = M3951()
    Case 3952: MC9 = M3952()
    Case 3953: MC9 = M3953()
    Case 3954: MC9 = M3954()
    Case 3955: MC9 = M3955()
    Case 3956: MC9 = M3956()
    Case 3957: MC9 = M3957()
    Case 3958: MC9 = M3958()
    Case 3959: MC9 = M3959()
    Case 3960: MC9 = M3960()
    Case 3961: MC9 = M3961()
    Case 3962: MC9 = M3962()
    Case 3963: MC9 = M3963()
    Case 3964: MC9 = M3964()
    Case 3965: MC9 = M3965()
    Case 3966: MC9 = M3966()
    Case 3967: MC9 = M3967()
    Case 3968: MC9 = M3968()
    Case 3969: MC9 = M3969()
    Case 3970: MC9 = M3970()
    Case 3971: MC9 = M3971()
    Case 3972: MC9 = M3972()
    Case 3973: MC9 = M3973()
    Case 3974: MC9 = M3974()
    Case 3975: MC9 = M3975()
    Case 3976: MC9 = M3976()
    Case 3977: MC9 = M3977()
    Case 3978: MC9 = M3978()
    Case 3979: MC9 = M3979()
    Case 3980: MC9 = M3980()
    Case 3981: MC9 = M3981()
    Case 3982: MC9 = M3982()
    Case 3983: MC9 = M3983()
    Case 3984: MC9 = M3984()
    Case 3985: MC9 = M3985()
    Case 3986: MC9 = M3986()
    Case 3987: MC9 = M3987()
    Case 3988: MC9 = M3988()
    Case 3989: MC9 = M3989()
    Case 3990: MC9 = M3990()
    Case 3991: MC9 = M3991()
    Case 3992: MC9 = M3992()
    Case 3993: MC9 = M3993()
    Case 3994: MC9 = M3994()
    Case 3995: MC9 = M3995()
    Case 3996: MC9 = M3996()
    Case 3997: MC9 = M3997()
    Case 3998: MC9 = M3998()
    Case 3999: MC9 = M3999()
    End Select
    Exit Function
EH:
    MC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3000() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M3000 = R
    Exit Function
EH:
    M3000 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3001() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M3001 = R
    Exit Function
EH:
    M3001 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3002() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3002 = R
    Exit Function
EH:
    M3002 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3003() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3003 = R
    Exit Function
EH:
    M3003 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3004() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3004 = R
    Exit Function
EH:
    M3004 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3005() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3005 = R
    Exit Function
EH:
    M3005 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3006() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3006 = R
    Exit Function
EH:
    M3006 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3007() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3007 = R
    Exit Function
EH:
    M3007 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3008() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3008 = R
    Exit Function
EH:
    M3008 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3009() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3009 = R
    Exit Function
EH:
    M3009 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3010() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3010 = R
    Exit Function
EH:
    M3010 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3011() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3011 = R
    Exit Function
EH:
    M3011 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3012() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3012 = R
    Exit Function
EH:
    M3012 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3013() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3013 = R
    Exit Function
EH:
    M3013 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3014() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3014 = R
    Exit Function
EH:
    M3014 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3015() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3015 = R
    Exit Function
EH:
    M3015 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3016() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3016 = R
    Exit Function
EH:
    M3016 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3017() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3017 = R
    Exit Function
EH:
    M3017 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3018() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3018 = R
    Exit Function
EH:
    M3018 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3019() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3019 = R
    Exit Function
EH:
    M3019 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3020() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3020 = R
    Exit Function
EH:
    M3020 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3021() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", 0)))
    M3021 = R
    Exit Function
EH:
    M3021 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3022() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3022 = R
    Exit Function
EH:
    M3022 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3023() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3023 = R
    Exit Function
EH:
    M3023 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3024() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3024 = R
    Exit Function
EH:
    M3024 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3025() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3025 = R
    Exit Function
EH:
    M3025 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3026() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3026 = R
    Exit Function
EH:
    M3026 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3027() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3027 = R
    Exit Function
EH:
    M3027 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3028() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3028 = R
    Exit Function
EH:
    M3028 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3029() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3029 = R
    Exit Function
EH:
    M3029 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3030() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3030 = R
    Exit Function
EH:
    M3030 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3031() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3031 = R
    Exit Function
EH:
    M3031 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3032() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3032 = R
    Exit Function
EH:
    M3032 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3033() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3033 = R
    Exit Function
EH:
    M3033 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3034() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3034 = R
    Exit Function
EH:
    M3034 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3035() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3035 = R
    Exit Function
EH:
    M3035 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3036() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3036 = R
    Exit Function
EH:
    M3036 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3037() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3037 = R
    Exit Function
EH:
    M3037 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3038() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3038 = R
    Exit Function
EH:
    M3038 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3039() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3039 = R
    Exit Function
EH:
    M3039 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3040() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3040 = R
    Exit Function
EH:
    M3040 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3041() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", 5)))
    M3041 = R
    Exit Function
EH:
    M3041 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3042() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3042 = R
    Exit Function
EH:
    M3042 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3043() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3043 = R
    Exit Function
EH:
    M3043 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3044() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3044 = R
    Exit Function
EH:
    M3044 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3045() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3045 = R
    Exit Function
EH:
    M3045 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3046() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3046 = R
    Exit Function
EH:
    M3046 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3047() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3047 = R
    Exit Function
EH:
    M3047 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3048() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3048 = R
    Exit Function
EH:
    M3048 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3049() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3049 = R
    Exit Function
EH:
    M3049 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3050() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3050 = R
    Exit Function
EH:
    M3050 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3051() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3051 = R
    Exit Function
EH:
    M3051 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3052() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3052 = R
    Exit Function
EH:
    M3052 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3053() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3053 = R
    Exit Function
EH:
    M3053 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3054() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3054 = R
    Exit Function
EH:
    M3054 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3055() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3055 = R
    Exit Function
EH:
    M3055 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3056() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3056 = R
    Exit Function
EH:
    M3056 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3057() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3057 = R
    Exit Function
EH:
    M3057 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3058() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3058 = R
    Exit Function
EH:
    M3058 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3059() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3059 = R
    Exit Function
EH:
    M3059 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3060() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3060 = R
    Exit Function
EH:
    M3060 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3061() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", 1, -2)))
    M3061 = R
    Exit Function
EH:
    M3061 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3062() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3062 = R
    Exit Function
EH:
    M3062 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3063() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3063 = R
    Exit Function
EH:
    M3063 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3064() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3064 = R
    Exit Function
EH:
    M3064 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3065() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3065 = R
    Exit Function
EH:
    M3065 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3066() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3066 = R
    Exit Function
EH:
    M3066 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3067() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3067 = R
    Exit Function
EH:
    M3067 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3068() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3068 = R
    Exit Function
EH:
    M3068 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3069() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3069 = R
    Exit Function
EH:
    M3069 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3070() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3070 = R
    Exit Function
EH:
    M3070 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3071() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3071 = R
    Exit Function
EH:
    M3071 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3072() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3072 = R
    Exit Function
EH:
    M3072 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3073() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3073 = R
    Exit Function
EH:
    M3073 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3074() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3074 = R
    Exit Function
EH:
    M3074 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3075() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3075 = R
    Exit Function
EH:
    M3075 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3076() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3076 = R
    Exit Function
EH:
    M3076 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3077() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3077 = R
    Exit Function
EH:
    M3077 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3078() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3078 = R
    Exit Function
EH:
    M3078 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3079() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3079 = R
    Exit Function
EH:
    M3079 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3080() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3080 = R
    Exit Function
EH:
    M3080 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3081() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", , 2)))
    M3081 = R
    Exit Function
EH:
    M3081 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3082() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3082 = R
    Exit Function
EH:
    M3082 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3083() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3083 = R
    Exit Function
EH:
    M3083 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3084() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3084 = R
    Exit Function
EH:
    M3084 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3085() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3085 = R
    Exit Function
EH:
    M3085 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3086() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3086 = R
    Exit Function
EH:
    M3086 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3087() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3087 = R
    Exit Function
EH:
    M3087 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3088() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3088 = R
    Exit Function
EH:
    M3088 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3089() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3089 = R
    Exit Function
EH:
    M3089 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3090() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3090 = R
    Exit Function
EH:
    M3090 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3091() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3091 = R
    Exit Function
EH:
    M3091 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3092() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3092 = R
    Exit Function
EH:
    M3092 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3093() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3093 = R
    Exit Function
EH:
    M3093 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3094() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3094 = R
    Exit Function
EH:
    M3094 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3095() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3095 = R
    Exit Function
EH:
    M3095 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3096() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3096 = R
    Exit Function
EH:
    M3096 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3097() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3097 = R
    Exit Function
EH:
    M3097 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3098() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3098 = R
    Exit Function
EH:
    M3098 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3099() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3099 = R
    Exit Function
EH:
    M3099 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3100 = R
    Exit Function
EH:
    M3100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Join(Split(s, "b"), "-")))
    M3101 = R
    Exit Function
EH:
    M3101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3102 = R
    Exit Function
EH:
    M3102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3103 = R
    Exit Function
EH:
    M3103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3104 = R
    Exit Function
EH:
    M3104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3105 = R
    Exit Function
EH:
    M3105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3106 = R
    Exit Function
EH:
    M3106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3107 = R
    Exit Function
EH:
    M3107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3108 = R
    Exit Function
EH:
    M3108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3109 = R
    Exit Function
EH:
    M3109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3110 = R
    Exit Function
EH:
    M3110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3111 = R
    Exit Function
EH:
    M3111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3112 = R
    Exit Function
EH:
    M3112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3113 = R
    Exit Function
EH:
    M3113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3114 = R
    Exit Function
EH:
    M3114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3115 = R
    Exit Function
EH:
    M3115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3116 = R
    Exit Function
EH:
    M3116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3117 = R
    Exit Function
EH:
    M3117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3118 = R
    Exit Function
EH:
    M3118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3119 = R
    Exit Function
EH:
    M3119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3120() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3120 = R
    Exit Function
EH:
    M3120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3121() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Join(Split(s, "B", , 1), "-")))
    M3121 = R
    Exit Function
EH:
    M3121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3122() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s))))
    M3122 = R
    Exit Function
EH:
    M3122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3123() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s))))
    M3123 = R
    Exit Function
EH:
    M3123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3124() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s))))
    M3124 = R
    Exit Function
EH:
    M3124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3125() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s))))
    M3125 = R
    Exit Function
EH:
    M3125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3126() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s))))
    M3126 = R
    Exit Function
EH:
    M3126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3127() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s))))
    M3127 = R
    Exit Function
EH:
    M3127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3128() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s))))
    M3128 = R
    Exit Function
EH:
    M3128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3129() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s))))
    M3129 = R
    Exit Function
EH:
    M3129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3130() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s))))
    M3130 = R
    Exit Function
EH:
    M3130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3131() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s))))
    M3131 = R
    Exit Function
EH:
    M3131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3132() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s))))
    M3132 = R
    Exit Function
EH:
    M3132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3133() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s))))
    M3133 = R
    Exit Function
EH:
    M3133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3134() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s))))
    M3134 = R
    Exit Function
EH:
    M3134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3135() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s))))
    M3135 = R
    Exit Function
EH:
    M3135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3136() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s))))
    M3136 = R
    Exit Function
EH:
    M3136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3137() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s))))
    M3137 = R
    Exit Function
EH:
    M3137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3138() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s))))
    M3138 = R
    Exit Function
EH:
    M3138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3139() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s))))
    M3139 = R
    Exit Function
EH:
    M3139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3140() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s))))
    M3140 = R
    Exit Function
EH:
    M3140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3141() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s))))
    M3141 = R
    Exit Function
EH:
    M3141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3142() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ","))))
    M3142 = R
    Exit Function
EH:
    M3142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3143() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ","))))
    M3143 = R
    Exit Function
EH:
    M3143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3144() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ","))))
    M3144 = R
    Exit Function
EH:
    M3144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3145() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ","))))
    M3145 = R
    Exit Function
EH:
    M3145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3146() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ","))))
    M3146 = R
    Exit Function
EH:
    M3146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3147() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ","))))
    M3147 = R
    Exit Function
EH:
    M3147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3148() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ","))))
    M3148 = R
    Exit Function
EH:
    M3148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3149() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ","))))
    M3149 = R
    Exit Function
EH:
    M3149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3150() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ","))))
    M3150 = R
    Exit Function
EH:
    M3150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3151() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ","))))
    M3151 = R
    Exit Function
EH:
    M3151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3152() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ","))))
    M3152 = R
    Exit Function
EH:
    M3152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3153() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ","))))
    M3153 = R
    Exit Function
EH:
    M3153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3154() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ","))))
    M3154 = R
    Exit Function
EH:
    M3154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3155() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ","))))
    M3155 = R
    Exit Function
EH:
    M3155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3156() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ","))))
    M3156 = R
    Exit Function
EH:
    M3156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3157() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ","))))
    M3157 = R
    Exit Function
EH:
    M3157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3158() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ","))))
    M3158 = R
    Exit Function
EH:
    M3158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3159() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ","))))
    M3159 = R
    Exit Function
EH:
    M3159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3160() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ","))))
    M3160 = R
    Exit Function
EH:
    M3160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3161() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ","))))
    M3161 = R
    Exit Function
EH:
    M3161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3162() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3162 = R
    Exit Function
EH:
    M3162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3163() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3163 = R
    Exit Function
EH:
    M3163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3164() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3164 = R
    Exit Function
EH:
    M3164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3165() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3165 = R
    Exit Function
EH:
    M3165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3166() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3166 = R
    Exit Function
EH:
    M3166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3167() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3167 = R
    Exit Function
EH:
    M3167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3168() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3168 = R
    Exit Function
EH:
    M3168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3169() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3169 = R
    Exit Function
EH:
    M3169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3170() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3170 = R
    Exit Function
EH:
    M3170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3171() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3171 = R
    Exit Function
EH:
    M3171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3172() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3172 = R
    Exit Function
EH:
    M3172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3173() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3173 = R
    Exit Function
EH:
    M3173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3174() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3174 = R
    Exit Function
EH:
    M3174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3175() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3175 = R
    Exit Function
EH:
    M3175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3176() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3176 = R
    Exit Function
EH:
    M3176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3177() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3177 = R
    Exit Function
EH:
    M3177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3178() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3178 = R
    Exit Function
EH:
    M3178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3179() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3179 = R
    Exit Function
EH:
    M3179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3180() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3180 = R
    Exit Function
EH:
    M3180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3181() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ",", 2))))
    M3181 = R
    Exit Function
EH:
    M3181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3182() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3182 = R
    Exit Function
EH:
    M3182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3183() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3183 = R
    Exit Function
EH:
    M3183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3184() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3184 = R
    Exit Function
EH:
    M3184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3185() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3185 = R
    Exit Function
EH:
    M3185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3186() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3186 = R
    Exit Function
EH:
    M3186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3187() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3187 = R
    Exit Function
EH:
    M3187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3188() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3188 = R
    Exit Function
EH:
    M3188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3189() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3189 = R
    Exit Function
EH:
    M3189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3190() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3190 = R
    Exit Function
EH:
    M3190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3191() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3191 = R
    Exit Function
EH:
    M3191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3192() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3192 = R
    Exit Function
EH:
    M3192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3193() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3193 = R
    Exit Function
EH:
    M3193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3194() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3194 = R
    Exit Function
EH:
    M3194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3195() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3195 = R
    Exit Function
EH:
    M3195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3196() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3196 = R
    Exit Function
EH:
    M3196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3197() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3197 = R
    Exit Function
EH:
    M3197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3198() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3198 = R
    Exit Function
EH:
    M3198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3199() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3199 = R
    Exit Function
EH:
    M3199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3200() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3200 = R
    Exit Function
EH:
    M3200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3201() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ",", 0))))
    M3201 = R
    Exit Function
EH:
    M3201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3202() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3202 = R
    Exit Function
EH:
    M3202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3203() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3203 = R
    Exit Function
EH:
    M3203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3204() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3204 = R
    Exit Function
EH:
    M3204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3205() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3205 = R
    Exit Function
EH:
    M3205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3206() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3206 = R
    Exit Function
EH:
    M3206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3207() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3207 = R
    Exit Function
EH:
    M3207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3208() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3208 = R
    Exit Function
EH:
    M3208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3209() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3209 = R
    Exit Function
EH:
    M3209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3210() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3210 = R
    Exit Function
EH:
    M3210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3211() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3211 = R
    Exit Function
EH:
    M3211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3212() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3212 = R
    Exit Function
EH:
    M3212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3213() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3213 = R
    Exit Function
EH:
    M3213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3214() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3214 = R
    Exit Function
EH:
    M3214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3215() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3215 = R
    Exit Function
EH:
    M3215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3216() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3216 = R
    Exit Function
EH:
    M3216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3217() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3217 = R
    Exit Function
EH:
    M3217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3218() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3218 = R
    Exit Function
EH:
    M3218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3219() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3219 = R
    Exit Function
EH:
    M3219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3220() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3220 = R
    Exit Function
EH:
    M3220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3221() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ",", 1))))
    M3221 = R
    Exit Function
EH:
    M3221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3222() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ""))))
    M3222 = R
    Exit Function
EH:
    M3222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3223() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ""))))
    M3223 = R
    Exit Function
EH:
    M3223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3224() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ""))))
    M3224 = R
    Exit Function
EH:
    M3224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3225() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ""))))
    M3225 = R
    Exit Function
EH:
    M3225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3226() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ""))))
    M3226 = R
    Exit Function
EH:
    M3226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3227() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ""))))
    M3227 = R
    Exit Function
EH:
    M3227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3228() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ""))))
    M3228 = R
    Exit Function
EH:
    M3228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3229() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ""))))
    M3229 = R
    Exit Function
EH:
    M3229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3230() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ""))))
    M3230 = R
    Exit Function
EH:
    M3230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3231() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ""))))
    M3231 = R
    Exit Function
EH:
    M3231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3232() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ""))))
    M3232 = R
    Exit Function
EH:
    M3232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3233() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ""))))
    M3233 = R
    Exit Function
EH:
    M3233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3234() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ""))))
    M3234 = R
    Exit Function
EH:
    M3234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3235() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ""))))
    M3235 = R
    Exit Function
EH:
    M3235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3236() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ""))))
    M3236 = R
    Exit Function
EH:
    M3236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3237() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ""))))
    M3237 = R
    Exit Function
EH:
    M3237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3238() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ""))))
    M3238 = R
    Exit Function
EH:
    M3238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3239() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ""))))
    M3239 = R
    Exit Function
EH:
    M3239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3240() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ""))))
    M3240 = R
    Exit Function
EH:
    M3240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3241() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ""))))
    M3241 = R
    Exit Function
EH:
    M3241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3242() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3242 = R
    Exit Function
EH:
    M3242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3243() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3243 = R
    Exit Function
EH:
    M3243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3244() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3244 = R
    Exit Function
EH:
    M3244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3245() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3245 = R
    Exit Function
EH:
    M3245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3246() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3246 = R
    Exit Function
EH:
    M3246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3247() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3247 = R
    Exit Function
EH:
    M3247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3248() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3248 = R
    Exit Function
EH:
    M3248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3249() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3249 = R
    Exit Function
EH:
    M3249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3250() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3250 = R
    Exit Function
EH:
    M3250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3251() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3251 = R
    Exit Function
EH:
    M3251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3252() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3252 = R
    Exit Function
EH:
    M3252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3253() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3253 = R
    Exit Function
EH:
    M3253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3254() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3254 = R
    Exit Function
EH:
    M3254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3255() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3255 = R
    Exit Function
EH:
    M3255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3256() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3256 = R
    Exit Function
EH:
    M3256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3257() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3257 = R
    Exit Function
EH:
    M3257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3258() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3258 = R
    Exit Function
EH:
    M3258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3259() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3259 = R
    Exit Function
EH:
    M3259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3260() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3260 = R
    Exit Function
EH:
    M3260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3261() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UBound(Split(s, ",", -1, 1))))
    M3261 = R
    Exit Function
EH:
    M3261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3262() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3262 = R
    Exit Function
EH:
    M3262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3263() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3263 = R
    Exit Function
EH:
    M3263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3264() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3264 = R
    Exit Function
EH:
    M3264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3265() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3265 = R
    Exit Function
EH:
    M3265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3266() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3266 = R
    Exit Function
EH:
    M3266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3267() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3267 = R
    Exit Function
EH:
    M3267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3268() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3268 = R
    Exit Function
EH:
    M3268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3269() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3269 = R
    Exit Function
EH:
    M3269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3270() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3270 = R
    Exit Function
EH:
    M3270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3271() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3271 = R
    Exit Function
EH:
    M3271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3272() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3272 = R
    Exit Function
EH:
    M3272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3273() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3273 = R
    Exit Function
EH:
    M3273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3274() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3274 = R
    Exit Function
EH:
    M3274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3275() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3275 = R
    Exit Function
EH:
    M3275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3276() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3276 = R
    Exit Function
EH:
    M3276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3277() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3277 = R
    Exit Function
EH:
    M3277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3278() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3278 = R
    Exit Function
EH:
    M3278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3279() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3279 = R
    Exit Function
EH:
    M3279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3280() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3280 = R
    Exit Function
EH:
    M3280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3281() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Join(Split(s, ",", -1), "|")))
    M3281 = R
    Exit Function
EH:
    M3281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3282() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3282 = R
    Exit Function
EH:
    M3282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3283() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3283 = R
    Exit Function
EH:
    M3283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3284() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3284 = R
    Exit Function
EH:
    M3284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3285() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3285 = R
    Exit Function
EH:
    M3285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3286() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3286 = R
    Exit Function
EH:
    M3286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3287() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3287 = R
    Exit Function
EH:
    M3287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3288() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3288 = R
    Exit Function
EH:
    M3288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3289() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3289 = R
    Exit Function
EH:
    M3289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3290() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3290 = R
    Exit Function
EH:
    M3290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3291() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3291 = R
    Exit Function
EH:
    M3291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3292() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3292 = R
    Exit Function
EH:
    M3292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3293() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3293 = R
    Exit Function
EH:
    M3293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3294() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3294 = R
    Exit Function
EH:
    M3294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3295() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3295 = R
    Exit Function
EH:
    M3295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3296() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3296 = R
    Exit Function
EH:
    M3296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3297() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3297 = R
    Exit Function
EH:
    M3297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3298() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3298 = R
    Exit Function
EH:
    M3298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3299() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3299 = R
    Exit Function
EH:
    M3299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3300() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3300 = R
    Exit Function
EH:
    M3300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3301() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Join(Split(s, "a", -1, 1), "|")))
    M3301 = R
    Exit Function
EH:
    M3301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3302() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrComp(s, "abc")))
    M3302 = R
    Exit Function
EH:
    M3302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3303() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrComp(s, "abc")))
    M3303 = R
    Exit Function
EH:
    M3303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3304() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrComp(s, "abc")))
    M3304 = R
    Exit Function
EH:
    M3304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3305() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrComp(s, "abc")))
    M3305 = R
    Exit Function
EH:
    M3305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3306() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrComp(s, "abc")))
    M3306 = R
    Exit Function
EH:
    M3306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3307() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrComp(s, "abc")))
    M3307 = R
    Exit Function
EH:
    M3307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3308() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrComp(s, "abc")))
    M3308 = R
    Exit Function
EH:
    M3308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3309() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrComp(s, "abc")))
    M3309 = R
    Exit Function
EH:
    M3309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3310() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrComp(s, "abc")))
    M3310 = R
    Exit Function
EH:
    M3310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3311() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrComp(s, "abc")))
    M3311 = R
    Exit Function
EH:
    M3311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3312() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrComp(s, "abc")))
    M3312 = R
    Exit Function
EH:
    M3312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3313() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrComp(s, "abc")))
    M3313 = R
    Exit Function
EH:
    M3313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3314() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrComp(s, "abc")))
    M3314 = R
    Exit Function
EH:
    M3314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3315() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrComp(s, "abc")))
    M3315 = R
    Exit Function
EH:
    M3315 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3316() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrComp(s, "abc")))
    M3316 = R
    Exit Function
EH:
    M3316 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3317() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrComp(s, "abc")))
    M3317 = R
    Exit Function
EH:
    M3317 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3318() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrComp(s, "abc")))
    M3318 = R
    Exit Function
EH:
    M3318 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3319() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrComp(s, "abc")))
    M3319 = R
    Exit Function
EH:
    M3319 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3320() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrComp(s, "abc")))
    M3320 = R
    Exit Function
EH:
    M3320 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3321() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrComp(s, "abc")))
    M3321 = R
    Exit Function
EH:
    M3321 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3322() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3322 = R
    Exit Function
EH:
    M3322 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3323() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3323 = R
    Exit Function
EH:
    M3323 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3324() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3324 = R
    Exit Function
EH:
    M3324 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3325() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3325 = R
    Exit Function
EH:
    M3325 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3326() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3326 = R
    Exit Function
EH:
    M3326 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3327() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3327 = R
    Exit Function
EH:
    M3327 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3328() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3328 = R
    Exit Function
EH:
    M3328 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3329() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3329 = R
    Exit Function
EH:
    M3329 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3330() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3330 = R
    Exit Function
EH:
    M3330 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3331() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3331 = R
    Exit Function
EH:
    M3331 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3332() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3332 = R
    Exit Function
EH:
    M3332 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3333() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3333 = R
    Exit Function
EH:
    M3333 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3334() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3334 = R
    Exit Function
EH:
    M3334 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3335() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3335 = R
    Exit Function
EH:
    M3335 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3336() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3336 = R
    Exit Function
EH:
    M3336 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3337() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3337 = R
    Exit Function
EH:
    M3337 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3338() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3338 = R
    Exit Function
EH:
    M3338 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3339() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3339 = R
    Exit Function
EH:
    M3339 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3340() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3340 = R
    Exit Function
EH:
    M3340 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3341() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrComp(s, "ABC", 1)))
    M3341 = R
    Exit Function
EH:
    M3341 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3342() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3342 = R
    Exit Function
EH:
    M3342 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3343() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3343 = R
    Exit Function
EH:
    M3343 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3344() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3344 = R
    Exit Function
EH:
    M3344 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3345() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3345 = R
    Exit Function
EH:
    M3345 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3346() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3346 = R
    Exit Function
EH:
    M3346 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3347() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3347 = R
    Exit Function
EH:
    M3347 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3348() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3348 = R
    Exit Function
EH:
    M3348 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3349() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3349 = R
    Exit Function
EH:
    M3349 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3350() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3350 = R
    Exit Function
EH:
    M3350 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3351() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3351 = R
    Exit Function
EH:
    M3351 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3352() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3352 = R
    Exit Function
EH:
    M3352 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3353() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3353 = R
    Exit Function
EH:
    M3353 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3354() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3354 = R
    Exit Function
EH:
    M3354 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3355() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3355 = R
    Exit Function
EH:
    M3355 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3356() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3356 = R
    Exit Function
EH:
    M3356 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3357() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3357 = R
    Exit Function
EH:
    M3357 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3358() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3358 = R
    Exit Function
EH:
    M3358 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3359() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3359 = R
    Exit Function
EH:
    M3359 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3360() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3360 = R
    Exit Function
EH:
    M3360 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3361() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrComp(s, "ABC", 0)))
    M3361 = R
    Exit Function
EH:
    M3361 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3362() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3362 = R
    Exit Function
EH:
    M3362 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3363() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3363 = R
    Exit Function
EH:
    M3363 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3364() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3364 = R
    Exit Function
EH:
    M3364 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3365() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3365 = R
    Exit Function
EH:
    M3365 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3366() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3366 = R
    Exit Function
EH:
    M3366 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3367() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3367 = R
    Exit Function
EH:
    M3367 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3368() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3368 = R
    Exit Function
EH:
    M3368 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3369() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3369 = R
    Exit Function
EH:
    M3369 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3370() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3370 = R
    Exit Function
EH:
    M3370 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3371() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3371 = R
    Exit Function
EH:
    M3371 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3372() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3372 = R
    Exit Function
EH:
    M3372 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3373() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3373 = R
    Exit Function
EH:
    M3373 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3374() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3374 = R
    Exit Function
EH:
    M3374 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3375() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3375 = R
    Exit Function
EH:
    M3375 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3376() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3376 = R
    Exit Function
EH:
    M3376 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3377() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3377 = R
    Exit Function
EH:
    M3377 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3378() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3378 = R
    Exit Function
EH:
    M3378 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3379() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3379 = R
    Exit Function
EH:
    M3379 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3380() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3380 = R
    Exit Function
EH:
    M3380 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3381() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrComp(s, "abc", 2)))
    M3381 = R
    Exit Function
EH:
    M3381 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3382() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrComp(s, "")))
    M3382 = R
    Exit Function
EH:
    M3382 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3383() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrComp(s, "")))
    M3383 = R
    Exit Function
EH:
    M3383 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3384() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrComp(s, "")))
    M3384 = R
    Exit Function
EH:
    M3384 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3385() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrComp(s, "")))
    M3385 = R
    Exit Function
EH:
    M3385 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3386() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrComp(s, "")))
    M3386 = R
    Exit Function
EH:
    M3386 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3387() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrComp(s, "")))
    M3387 = R
    Exit Function
EH:
    M3387 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3388() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrComp(s, "")))
    M3388 = R
    Exit Function
EH:
    M3388 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3389() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrComp(s, "")))
    M3389 = R
    Exit Function
EH:
    M3389 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3390() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrComp(s, "")))
    M3390 = R
    Exit Function
EH:
    M3390 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3391() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrComp(s, "")))
    M3391 = R
    Exit Function
EH:
    M3391 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3392() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrComp(s, "")))
    M3392 = R
    Exit Function
EH:
    M3392 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3393() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrComp(s, "")))
    M3393 = R
    Exit Function
EH:
    M3393 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3394() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrComp(s, "")))
    M3394 = R
    Exit Function
EH:
    M3394 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3395() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrComp(s, "")))
    M3395 = R
    Exit Function
EH:
    M3395 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3396() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrComp(s, "")))
    M3396 = R
    Exit Function
EH:
    M3396 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3397() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrComp(s, "")))
    M3397 = R
    Exit Function
EH:
    M3397 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3398() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrComp(s, "")))
    M3398 = R
    Exit Function
EH:
    M3398 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3399() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrComp(s, "")))
    M3399 = R
    Exit Function
EH:
    M3399 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3400() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrComp(s, "")))
    M3400 = R
    Exit Function
EH:
    M3400 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3401() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrComp(s, "")))
    M3401 = R
    Exit Function
EH:
    M3401 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3402() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Left(s, 0) = ""))
    M3402 = R
    Exit Function
EH:
    M3402 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3403() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Left(s, 0) = ""))
    M3403 = R
    Exit Function
EH:
    M3403 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3404() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Left(s, 0) = ""))
    M3404 = R
    Exit Function
EH:
    M3404 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3405() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Left(s, 0) = ""))
    M3405 = R
    Exit Function
EH:
    M3405 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3406() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Left(s, 0) = ""))
    M3406 = R
    Exit Function
EH:
    M3406 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3407() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Left(s, 0) = ""))
    M3407 = R
    Exit Function
EH:
    M3407 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3408() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Left(s, 0) = ""))
    M3408 = R
    Exit Function
EH:
    M3408 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3409() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Left(s, 0) = ""))
    M3409 = R
    Exit Function
EH:
    M3409 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3410() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Left(s, 0) = ""))
    M3410 = R
    Exit Function
EH:
    M3410 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3411() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Left(s, 0) = ""))
    M3411 = R
    Exit Function
EH:
    M3411 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3412() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Left(s, 0) = ""))
    M3412 = R
    Exit Function
EH:
    M3412 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3413() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Left(s, 0) = ""))
    M3413 = R
    Exit Function
EH:
    M3413 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3414() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Left(s, 0) = ""))
    M3414 = R
    Exit Function
EH:
    M3414 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3415() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Left(s, 0) = ""))
    M3415 = R
    Exit Function
EH:
    M3415 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3416() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Left(s, 0) = ""))
    M3416 = R
    Exit Function
EH:
    M3416 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3417() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Left(s, 0) = ""))
    M3417 = R
    Exit Function
EH:
    M3417 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3418() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Left(s, 0) = ""))
    M3418 = R
    Exit Function
EH:
    M3418 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3419() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Left(s, 0) = ""))
    M3419 = R
    Exit Function
EH:
    M3419 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3420() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Left(s, 0) = ""))
    M3420 = R
    Exit Function
EH:
    M3420 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3421() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Left(s, 0) = ""))
    M3421 = R
    Exit Function
EH:
    M3421 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3422() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Left$(s, -1)))
    M3422 = R
    Exit Function
EH:
    M3422 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3423() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Left$(s, -1)))
    M3423 = R
    Exit Function
EH:
    M3423 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3424() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Left$(s, -1)))
    M3424 = R
    Exit Function
EH:
    M3424 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3425() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Left$(s, -1)))
    M3425 = R
    Exit Function
EH:
    M3425 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3426() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Left$(s, -1)))
    M3426 = R
    Exit Function
EH:
    M3426 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3427() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Left$(s, -1)))
    M3427 = R
    Exit Function
EH:
    M3427 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3428() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Left$(s, -1)))
    M3428 = R
    Exit Function
EH:
    M3428 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3429() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Left$(s, -1)))
    M3429 = R
    Exit Function
EH:
    M3429 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3430() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Left$(s, -1)))
    M3430 = R
    Exit Function
EH:
    M3430 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3431() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Left$(s, -1)))
    M3431 = R
    Exit Function
EH:
    M3431 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3432() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Left$(s, -1)))
    M3432 = R
    Exit Function
EH:
    M3432 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3433() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Left$(s, -1)))
    M3433 = R
    Exit Function
EH:
    M3433 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3434() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Left$(s, -1)))
    M3434 = R
    Exit Function
EH:
    M3434 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3435() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Left$(s, -1)))
    M3435 = R
    Exit Function
EH:
    M3435 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3436() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Left$(s, -1)))
    M3436 = R
    Exit Function
EH:
    M3436 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3437() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Left$(s, -1)))
    M3437 = R
    Exit Function
EH:
    M3437 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3438() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Left$(s, -1)))
    M3438 = R
    Exit Function
EH:
    M3438 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3439() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Left$(s, -1)))
    M3439 = R
    Exit Function
EH:
    M3439 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3440() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Left$(s, -1)))
    M3440 = R
    Exit Function
EH:
    M3440 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3441() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Left$(s, -1)))
    M3441 = R
    Exit Function
EH:
    M3441 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3442() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Mid$(s, 0)))
    M3442 = R
    Exit Function
EH:
    M3442 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3443() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Mid$(s, 0)))
    M3443 = R
    Exit Function
EH:
    M3443 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3444() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Mid$(s, 0)))
    M3444 = R
    Exit Function
EH:
    M3444 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3445() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Mid$(s, 0)))
    M3445 = R
    Exit Function
EH:
    M3445 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3446() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Mid$(s, 0)))
    M3446 = R
    Exit Function
EH:
    M3446 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3447() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Mid$(s, 0)))
    M3447 = R
    Exit Function
EH:
    M3447 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3448() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Mid$(s, 0)))
    M3448 = R
    Exit Function
EH:
    M3448 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3449() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Mid$(s, 0)))
    M3449 = R
    Exit Function
EH:
    M3449 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3450() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Mid$(s, 0)))
    M3450 = R
    Exit Function
EH:
    M3450 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3451() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Mid$(s, 0)))
    M3451 = R
    Exit Function
EH:
    M3451 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3452() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Mid$(s, 0)))
    M3452 = R
    Exit Function
EH:
    M3452 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3453() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Mid$(s, 0)))
    M3453 = R
    Exit Function
EH:
    M3453 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3454() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Mid$(s, 0)))
    M3454 = R
    Exit Function
EH:
    M3454 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3455() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Mid$(s, 0)))
    M3455 = R
    Exit Function
EH:
    M3455 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3456() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Mid$(s, 0)))
    M3456 = R
    Exit Function
EH:
    M3456 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3457() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Mid$(s, 0)))
    M3457 = R
    Exit Function
EH:
    M3457 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3458() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Mid$(s, 0)))
    M3458 = R
    Exit Function
EH:
    M3458 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3459() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Mid$(s, 0)))
    M3459 = R
    Exit Function
EH:
    M3459 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3460() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Mid$(s, 0)))
    M3460 = R
    Exit Function
EH:
    M3460 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3461() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Mid$(s, 0)))
    M3461 = R
    Exit Function
EH:
    M3461 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3462() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Mid$(s, 100)))
    M3462 = R
    Exit Function
EH:
    M3462 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3463() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Mid$(s, 100)))
    M3463 = R
    Exit Function
EH:
    M3463 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3464() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Mid$(s, 100)))
    M3464 = R
    Exit Function
EH:
    M3464 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3465() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Mid$(s, 100)))
    M3465 = R
    Exit Function
EH:
    M3465 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3466() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Mid$(s, 100)))
    M3466 = R
    Exit Function
EH:
    M3466 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3467() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Mid$(s, 100)))
    M3467 = R
    Exit Function
EH:
    M3467 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3468() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Mid$(s, 100)))
    M3468 = R
    Exit Function
EH:
    M3468 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3469() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Mid$(s, 100)))
    M3469 = R
    Exit Function
EH:
    M3469 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3470() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Mid$(s, 100)))
    M3470 = R
    Exit Function
EH:
    M3470 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3471() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Mid$(s, 100)))
    M3471 = R
    Exit Function
EH:
    M3471 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3472() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Mid$(s, 100)))
    M3472 = R
    Exit Function
EH:
    M3472 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3473() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Mid$(s, 100)))
    M3473 = R
    Exit Function
EH:
    M3473 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3474() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Mid$(s, 100)))
    M3474 = R
    Exit Function
EH:
    M3474 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3475() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Mid$(s, 100)))
    M3475 = R
    Exit Function
EH:
    M3475 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3476() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Mid$(s, 100)))
    M3476 = R
    Exit Function
EH:
    M3476 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3477() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Mid$(s, 100)))
    M3477 = R
    Exit Function
EH:
    M3477 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3478() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Mid$(s, 100)))
    M3478 = R
    Exit Function
EH:
    M3478 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3479() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Mid$(s, 100)))
    M3479 = R
    Exit Function
EH:
    M3479 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3480() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Mid$(s, 100)))
    M3480 = R
    Exit Function
EH:
    M3480 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3481() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Mid$(s, 100)))
    M3481 = R
    Exit Function
EH:
    M3481 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3482() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Mid$(s, 1, -1)))
    M3482 = R
    Exit Function
EH:
    M3482 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3483() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3483 = R
    Exit Function
EH:
    M3483 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3484() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3484 = R
    Exit Function
EH:
    M3484 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3485() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3485 = R
    Exit Function
EH:
    M3485 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3486() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Mid$(s, 1, -1)))
    M3486 = R
    Exit Function
EH:
    M3486 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3487() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3487 = R
    Exit Function
EH:
    M3487 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3488() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3488 = R
    Exit Function
EH:
    M3488 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3489() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3489 = R
    Exit Function
EH:
    M3489 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3490() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3490 = R
    Exit Function
EH:
    M3490 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3491() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3491 = R
    Exit Function
EH:
    M3491 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3492() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Mid$(s, 1, -1)))
    M3492 = R
    Exit Function
EH:
    M3492 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3493() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3493 = R
    Exit Function
EH:
    M3493 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3494() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3494 = R
    Exit Function
EH:
    M3494 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3495() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3495 = R
    Exit Function
EH:
    M3495 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3496() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Mid$(s, 1, -1)))
    M3496 = R
    Exit Function
EH:
    M3496 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3497() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Mid$(s, 1, -1)))
    M3497 = R
    Exit Function
EH:
    M3497 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3498() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3498 = R
    Exit Function
EH:
    M3498 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3499() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3499 = R
    Exit Function
EH:
    M3499 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3500() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3500 = R
    Exit Function
EH:
    M3500 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3501() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Mid$(s, 1, -1)))
    M3501 = R
    Exit Function
EH:
    M3501 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3502() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Right$(s, 100)))
    M3502 = R
    Exit Function
EH:
    M3502 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3503() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Right$(s, 100)))
    M3503 = R
    Exit Function
EH:
    M3503 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3504() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Right$(s, 100)))
    M3504 = R
    Exit Function
EH:
    M3504 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3505() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Right$(s, 100)))
    M3505 = R
    Exit Function
EH:
    M3505 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3506() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Right$(s, 100)))
    M3506 = R
    Exit Function
EH:
    M3506 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3507() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Right$(s, 100)))
    M3507 = R
    Exit Function
EH:
    M3507 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3508() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Right$(s, 100)))
    M3508 = R
    Exit Function
EH:
    M3508 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3509() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Right$(s, 100)))
    M3509 = R
    Exit Function
EH:
    M3509 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3510() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Right$(s, 100)))
    M3510 = R
    Exit Function
EH:
    M3510 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3511() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Right$(s, 100)))
    M3511 = R
    Exit Function
EH:
    M3511 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3512() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Right$(s, 100)))
    M3512 = R
    Exit Function
EH:
    M3512 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3513() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Right$(s, 100)))
    M3513 = R
    Exit Function
EH:
    M3513 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3514() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Right$(s, 100)))
    M3514 = R
    Exit Function
EH:
    M3514 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3515() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Right$(s, 100)))
    M3515 = R
    Exit Function
EH:
    M3515 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3516() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Right$(s, 100)))
    M3516 = R
    Exit Function
EH:
    M3516 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3517() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Right$(s, 100)))
    M3517 = R
    Exit Function
EH:
    M3517 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3518() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Right$(s, 100)))
    M3518 = R
    Exit Function
EH:
    M3518 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3519() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Right$(s, 100)))
    M3519 = R
    Exit Function
EH:
    M3519 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3520() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Right$(s, 100)))
    M3520 = R
    Exit Function
EH:
    M3520 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3521() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Right$(s, 100)))
    M3521 = R
    Exit Function
EH:
    M3521 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3522() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Right$(s, -1)))
    M3522 = R
    Exit Function
EH:
    M3522 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3523() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Right$(s, -1)))
    M3523 = R
    Exit Function
EH:
    M3523 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3524() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Right$(s, -1)))
    M3524 = R
    Exit Function
EH:
    M3524 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3525() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Right$(s, -1)))
    M3525 = R
    Exit Function
EH:
    M3525 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3526() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Right$(s, -1)))
    M3526 = R
    Exit Function
EH:
    M3526 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3527() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Right$(s, -1)))
    M3527 = R
    Exit Function
EH:
    M3527 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3528() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Right$(s, -1)))
    M3528 = R
    Exit Function
EH:
    M3528 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3529() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Right$(s, -1)))
    M3529 = R
    Exit Function
EH:
    M3529 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3530() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Right$(s, -1)))
    M3530 = R
    Exit Function
EH:
    M3530 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3531() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Right$(s, -1)))
    M3531 = R
    Exit Function
EH:
    M3531 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3532() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Right$(s, -1)))
    M3532 = R
    Exit Function
EH:
    M3532 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3533() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Right$(s, -1)))
    M3533 = R
    Exit Function
EH:
    M3533 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3534() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Right$(s, -1)))
    M3534 = R
    Exit Function
EH:
    M3534 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3535() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Right$(s, -1)))
    M3535 = R
    Exit Function
EH:
    M3535 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3536() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Right$(s, -1)))
    M3536 = R
    Exit Function
EH:
    M3536 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3537() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Right$(s, -1)))
    M3537 = R
    Exit Function
EH:
    M3537 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3538() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Right$(s, -1)))
    M3538 = R
    Exit Function
EH:
    M3538 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3539() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Right$(s, -1)))
    M3539 = R
    Exit Function
EH:
    M3539 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3540() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Right$(s, -1)))
    M3540 = R
    Exit Function
EH:
    M3540 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3541() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Right$(s, -1)))
    M3541 = R
    Exit Function
EH:
    M3541 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3542() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3542 = R
    Exit Function
EH:
    M3542 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3543() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3543 = R
    Exit Function
EH:
    M3543 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3544() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3544 = R
    Exit Function
EH:
    M3544 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3545() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3545 = R
    Exit Function
EH:
    M3545 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3546() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3546 = R
    Exit Function
EH:
    M3546 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3547() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3547 = R
    Exit Function
EH:
    M3547 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3548() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3548 = R
    Exit Function
EH:
    M3548 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3549() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3549 = R
    Exit Function
EH:
    M3549 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3550() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3550 = R
    Exit Function
EH:
    M3550 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3551() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3551 = R
    Exit Function
EH:
    M3551 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3552() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3552 = R
    Exit Function
EH:
    M3552 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3553() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3553 = R
    Exit Function
EH:
    M3553 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3554() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3554 = R
    Exit Function
EH:
    M3554 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3555() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3555 = R
    Exit Function
EH:
    M3555 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3556() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3556 = R
    Exit Function
EH:
    M3556 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3557() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3557 = R
    Exit Function
EH:
    M3557 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3558() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3558 = R
    Exit Function
EH:
    M3558 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3559() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3559 = R
    Exit Function
EH:
    M3559 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3560() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3560 = R
    Exit Function
EH:
    M3560 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3561() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Filter(Array(s, "b"), "b")))
    M3561 = R
    Exit Function
EH:
    M3561 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3562() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3562 = R
    Exit Function
EH:
    M3562 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3563() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3563 = R
    Exit Function
EH:
    M3563 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3564() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3564 = R
    Exit Function
EH:
    M3564 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3565() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3565 = R
    Exit Function
EH:
    M3565 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3566() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3566 = R
    Exit Function
EH:
    M3566 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3567() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3567 = R
    Exit Function
EH:
    M3567 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3568() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3568 = R
    Exit Function
EH:
    M3568 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3569() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3569 = R
    Exit Function
EH:
    M3569 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3570() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3570 = R
    Exit Function
EH:
    M3570 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3571() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3571 = R
    Exit Function
EH:
    M3571 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3572() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3572 = R
    Exit Function
EH:
    M3572 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3573() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3573 = R
    Exit Function
EH:
    M3573 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3574() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3574 = R
    Exit Function
EH:
    M3574 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3575() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3575 = R
    Exit Function
EH:
    M3575 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3576() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3576 = R
    Exit Function
EH:
    M3576 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3577() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3577 = R
    Exit Function
EH:
    M3577 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3578() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3578 = R
    Exit Function
EH:
    M3578 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3579() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3579 = R
    Exit Function
EH:
    M3579 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3580() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3580 = R
    Exit Function
EH:
    M3580 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3581() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Filter(Array(s, "B"), "b", True, 1)))
    M3581 = R
    Exit Function
EH:
    M3581 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3582() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3582 = R
    Exit Function
EH:
    M3582 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3583() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3583 = R
    Exit Function
EH:
    M3583 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3584() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3584 = R
    Exit Function
EH:
    M3584 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3585() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3585 = R
    Exit Function
EH:
    M3585 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3586() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3586 = R
    Exit Function
EH:
    M3586 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3587() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3587 = R
    Exit Function
EH:
    M3587 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3588() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3588 = R
    Exit Function
EH:
    M3588 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3589() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3589 = R
    Exit Function
EH:
    M3589 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3590() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3590 = R
    Exit Function
EH:
    M3590 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3591() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3591 = R
    Exit Function
EH:
    M3591 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3592() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3592 = R
    Exit Function
EH:
    M3592 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3593() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3593 = R
    Exit Function
EH:
    M3593 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3594() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3594 = R
    Exit Function
EH:
    M3594 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3595() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3595 = R
    Exit Function
EH:
    M3595 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3596() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3596 = R
    Exit Function
EH:
    M3596 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3597() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3597 = R
    Exit Function
EH:
    M3597 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3598() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3598 = R
    Exit Function
EH:
    M3598 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3599() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3599 = R
    Exit Function
EH:
    M3599 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3600() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3600 = R
    Exit Function
EH:
    M3600 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3601() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Filter(Array(s, "B"), "b", False, 1)))
    M3601 = R
    Exit Function
EH:
    M3601 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3602() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(LCase(UCase(s))))
    M3602 = R
    Exit Function
EH:
    M3602 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3603() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(LCase(UCase(s))))
    M3603 = R
    Exit Function
EH:
    M3603 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3604() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(LCase(UCase(s))))
    M3604 = R
    Exit Function
EH:
    M3604 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3605() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(LCase(UCase(s))))
    M3605 = R
    Exit Function
EH:
    M3605 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3606() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(LCase(UCase(s))))
    M3606 = R
    Exit Function
EH:
    M3606 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3607() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(LCase(UCase(s))))
    M3607 = R
    Exit Function
EH:
    M3607 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3608() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(LCase(UCase(s))))
    M3608 = R
    Exit Function
EH:
    M3608 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3609() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(LCase(UCase(s))))
    M3609 = R
    Exit Function
EH:
    M3609 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3610() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(LCase(UCase(s))))
    M3610 = R
    Exit Function
EH:
    M3610 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3611() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(LCase(UCase(s))))
    M3611 = R
    Exit Function
EH:
    M3611 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3612() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(LCase(UCase(s))))
    M3612 = R
    Exit Function
EH:
    M3612 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3613() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(LCase(UCase(s))))
    M3613 = R
    Exit Function
EH:
    M3613 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3614() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(LCase(UCase(s))))
    M3614 = R
    Exit Function
EH:
    M3614 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3615() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(LCase(UCase(s))))
    M3615 = R
    Exit Function
EH:
    M3615 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3616() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(LCase(UCase(s))))
    M3616 = R
    Exit Function
EH:
    M3616 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3617() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(LCase(UCase(s))))
    M3617 = R
    Exit Function
EH:
    M3617 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3618() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(LCase(UCase(s))))
    M3618 = R
    Exit Function
EH:
    M3618 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3619() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(LCase(UCase(s))))
    M3619 = R
    Exit Function
EH:
    M3619 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3620() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(LCase(UCase(s))))
    M3620 = R
    Exit Function
EH:
    M3620 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3621() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(LCase(UCase(s))))
    M3621 = R
    Exit Function
EH:
    M3621 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3622() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(UCase(LCase(s))))
    M3622 = R
    Exit Function
EH:
    M3622 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3623() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(UCase(LCase(s))))
    M3623 = R
    Exit Function
EH:
    M3623 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3624() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(UCase(LCase(s))))
    M3624 = R
    Exit Function
EH:
    M3624 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3625() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(UCase(LCase(s))))
    M3625 = R
    Exit Function
EH:
    M3625 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3626() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(UCase(LCase(s))))
    M3626 = R
    Exit Function
EH:
    M3626 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3627() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(UCase(LCase(s))))
    M3627 = R
    Exit Function
EH:
    M3627 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3628() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(UCase(LCase(s))))
    M3628 = R
    Exit Function
EH:
    M3628 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3629() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(UCase(LCase(s))))
    M3629 = R
    Exit Function
EH:
    M3629 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3630() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(UCase(LCase(s))))
    M3630 = R
    Exit Function
EH:
    M3630 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3631() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(UCase(LCase(s))))
    M3631 = R
    Exit Function
EH:
    M3631 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3632() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(UCase(LCase(s))))
    M3632 = R
    Exit Function
EH:
    M3632 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3633() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(UCase(LCase(s))))
    M3633 = R
    Exit Function
EH:
    M3633 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3634() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(UCase(LCase(s))))
    M3634 = R
    Exit Function
EH:
    M3634 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3635() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(UCase(LCase(s))))
    M3635 = R
    Exit Function
EH:
    M3635 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3636() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(UCase(LCase(s))))
    M3636 = R
    Exit Function
EH:
    M3636 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3637() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(UCase(LCase(s))))
    M3637 = R
    Exit Function
EH:
    M3637 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3638() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(UCase(LCase(s))))
    M3638 = R
    Exit Function
EH:
    M3638 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3639() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(UCase(LCase(s))))
    M3639 = R
    Exit Function
EH:
    M3639 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3640() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(UCase(LCase(s))))
    M3640 = R
    Exit Function
EH:
    M3640 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3641() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(UCase(LCase(s))))
    M3641 = R
    Exit Function
EH:
    M3641 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3642() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Format(s, ">")))
    M3642 = R
    Exit Function
EH:
    M3642 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3643() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Format(s, ">")))
    M3643 = R
    Exit Function
EH:
    M3643 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3644() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Format(s, ">")))
    M3644 = R
    Exit Function
EH:
    M3644 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3645() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Format(s, ">")))
    M3645 = R
    Exit Function
EH:
    M3645 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3646() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Format(s, ">")))
    M3646 = R
    Exit Function
EH:
    M3646 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3647() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Format(s, ">")))
    M3647 = R
    Exit Function
EH:
    M3647 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3648() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Format(s, ">")))
    M3648 = R
    Exit Function
EH:
    M3648 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3649() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Format(s, ">")))
    M3649 = R
    Exit Function
EH:
    M3649 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3650() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Format(s, ">")))
    M3650 = R
    Exit Function
EH:
    M3650 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3651() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Format(s, ">")))
    M3651 = R
    Exit Function
EH:
    M3651 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3652() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Format(s, ">")))
    M3652 = R
    Exit Function
EH:
    M3652 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3653() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Format(s, ">")))
    M3653 = R
    Exit Function
EH:
    M3653 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3654() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Format(s, ">")))
    M3654 = R
    Exit Function
EH:
    M3654 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3655() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Format(s, ">")))
    M3655 = R
    Exit Function
EH:
    M3655 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3656() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Format(s, ">")))
    M3656 = R
    Exit Function
EH:
    M3656 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3657() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Format(s, ">")))
    M3657 = R
    Exit Function
EH:
    M3657 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3658() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Format(s, ">")))
    M3658 = R
    Exit Function
EH:
    M3658 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3659() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Format(s, ">")))
    M3659 = R
    Exit Function
EH:
    M3659 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3660() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Format(s, ">")))
    M3660 = R
    Exit Function
EH:
    M3660 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3661() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Format(s, ">")))
    M3661 = R
    Exit Function
EH:
    M3661 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3662() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Format(s, "<")))
    M3662 = R
    Exit Function
EH:
    M3662 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3663() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Format(s, "<")))
    M3663 = R
    Exit Function
EH:
    M3663 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3664() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Format(s, "<")))
    M3664 = R
    Exit Function
EH:
    M3664 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3665() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Format(s, "<")))
    M3665 = R
    Exit Function
EH:
    M3665 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3666() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Format(s, "<")))
    M3666 = R
    Exit Function
EH:
    M3666 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3667() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Format(s, "<")))
    M3667 = R
    Exit Function
EH:
    M3667 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3668() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Format(s, "<")))
    M3668 = R
    Exit Function
EH:
    M3668 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3669() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Format(s, "<")))
    M3669 = R
    Exit Function
EH:
    M3669 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3670() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Format(s, "<")))
    M3670 = R
    Exit Function
EH:
    M3670 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3671() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Format(s, "<")))
    M3671 = R
    Exit Function
EH:
    M3671 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3672() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Format(s, "<")))
    M3672 = R
    Exit Function
EH:
    M3672 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3673() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Format(s, "<")))
    M3673 = R
    Exit Function
EH:
    M3673 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3674() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Format(s, "<")))
    M3674 = R
    Exit Function
EH:
    M3674 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3675() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Format(s, "<")))
    M3675 = R
    Exit Function
EH:
    M3675 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3676() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Format(s, "<")))
    M3676 = R
    Exit Function
EH:
    M3676 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3677() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Format(s, "<")))
    M3677 = R
    Exit Function
EH:
    M3677 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3678() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Format(s, "<")))
    M3678 = R
    Exit Function
EH:
    M3678 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3679() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Format(s, "<")))
    M3679 = R
    Exit Function
EH:
    M3679 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3680() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Format(s, "<")))
    M3680 = R
    Exit Function
EH:
    M3680 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3681() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Format(s, "<")))
    M3681 = R
    Exit Function
EH:
    M3681 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3682() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Len(s & Null)))
    M3682 = R
    Exit Function
EH:
    M3682 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3683() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Len(s & Null)))
    M3683 = R
    Exit Function
EH:
    M3683 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3684() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Len(s & Null)))
    M3684 = R
    Exit Function
EH:
    M3684 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3685() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Len(s & Null)))
    M3685 = R
    Exit Function
EH:
    M3685 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3686() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Len(s & Null)))
    M3686 = R
    Exit Function
EH:
    M3686 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3687() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Len(s & Null)))
    M3687 = R
    Exit Function
EH:
    M3687 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3688() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Len(s & Null)))
    M3688 = R
    Exit Function
EH:
    M3688 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3689() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Len(s & Null)))
    M3689 = R
    Exit Function
EH:
    M3689 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3690() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Len(s & Null)))
    M3690 = R
    Exit Function
EH:
    M3690 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3691() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Len(s & Null)))
    M3691 = R
    Exit Function
EH:
    M3691 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3692() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Len(s & Null)))
    M3692 = R
    Exit Function
EH:
    M3692 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3693() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Len(s & Null)))
    M3693 = R
    Exit Function
EH:
    M3693 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3694() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Len(s & Null)))
    M3694 = R
    Exit Function
EH:
    M3694 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3695() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Len(s & Null)))
    M3695 = R
    Exit Function
EH:
    M3695 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3696() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Len(s & Null)))
    M3696 = R
    Exit Function
EH:
    M3696 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3697() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Len(s & Null)))
    M3697 = R
    Exit Function
EH:
    M3697 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3698() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Len(s & Null)))
    M3698 = R
    Exit Function
EH:
    M3698 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3699() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Len(s & Null)))
    M3699 = R
    Exit Function
EH:
    M3699 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3700() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Len(s & Null)))
    M3700 = R
    Exit Function
EH:
    M3700 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3701() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Len(s & Null)))
    M3701 = R
    Exit Function
EH:
    M3701 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3702() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Len(Null & s)))
    M3702 = R
    Exit Function
EH:
    M3702 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3703() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Len(Null & s)))
    M3703 = R
    Exit Function
EH:
    M3703 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3704() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Len(Null & s)))
    M3704 = R
    Exit Function
EH:
    M3704 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3705() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Len(Null & s)))
    M3705 = R
    Exit Function
EH:
    M3705 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3706() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Len(Null & s)))
    M3706 = R
    Exit Function
EH:
    M3706 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3707() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Len(Null & s)))
    M3707 = R
    Exit Function
EH:
    M3707 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3708() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Len(Null & s)))
    M3708 = R
    Exit Function
EH:
    M3708 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3709() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Len(Null & s)))
    M3709 = R
    Exit Function
EH:
    M3709 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3710() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Len(Null & s)))
    M3710 = R
    Exit Function
EH:
    M3710 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3711() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Len(Null & s)))
    M3711 = R
    Exit Function
EH:
    M3711 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3712() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Len(Null & s)))
    M3712 = R
    Exit Function
EH:
    M3712 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3713() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Len(Null & s)))
    M3713 = R
    Exit Function
EH:
    M3713 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3714() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Len(Null & s)))
    M3714 = R
    Exit Function
EH:
    M3714 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3715() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Len(Null & s)))
    M3715 = R
    Exit Function
EH:
    M3715 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3716() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Len(Null & s)))
    M3716 = R
    Exit Function
EH:
    M3716 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3717() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Len(Null & s)))
    M3717 = R
    Exit Function
EH:
    M3717 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3718() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Len(Null & s)))
    M3718 = R
    Exit Function
EH:
    M3718 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3719() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Len(Null & s)))
    M3719 = R
    Exit Function
EH:
    M3719 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3720() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Len(Null & s)))
    M3720 = R
    Exit Function
EH:
    M3720 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3721() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Len(Null & s)))
    M3721 = R
    Exit Function
EH:
    M3721 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3722() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(IsNull(s & Null)))
    M3722 = R
    Exit Function
EH:
    M3722 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3723() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(IsNull(s & Null)))
    M3723 = R
    Exit Function
EH:
    M3723 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3724() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(IsNull(s & Null)))
    M3724 = R
    Exit Function
EH:
    M3724 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3725() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(IsNull(s & Null)))
    M3725 = R
    Exit Function
EH:
    M3725 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3726() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(IsNull(s & Null)))
    M3726 = R
    Exit Function
EH:
    M3726 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3727() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(IsNull(s & Null)))
    M3727 = R
    Exit Function
EH:
    M3727 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3728() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(IsNull(s & Null)))
    M3728 = R
    Exit Function
EH:
    M3728 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3729() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(IsNull(s & Null)))
    M3729 = R
    Exit Function
EH:
    M3729 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3730() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(IsNull(s & Null)))
    M3730 = R
    Exit Function
EH:
    M3730 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3731() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(IsNull(s & Null)))
    M3731 = R
    Exit Function
EH:
    M3731 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3732() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(IsNull(s & Null)))
    M3732 = R
    Exit Function
EH:
    M3732 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3733() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(IsNull(s & Null)))
    M3733 = R
    Exit Function
EH:
    M3733 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3734() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(IsNull(s & Null)))
    M3734 = R
    Exit Function
EH:
    M3734 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3735() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(IsNull(s & Null)))
    M3735 = R
    Exit Function
EH:
    M3735 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3736() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(IsNull(s & Null)))
    M3736 = R
    Exit Function
EH:
    M3736 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3737() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(IsNull(s & Null)))
    M3737 = R
    Exit Function
EH:
    M3737 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3738() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(IsNull(s & Null)))
    M3738 = R
    Exit Function
EH:
    M3738 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3739() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(IsNull(s & Null)))
    M3739 = R
    Exit Function
EH:
    M3739 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3740() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(IsNull(s & Null)))
    M3740 = R
    Exit Function
EH:
    M3740 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3741() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(IsNull(s & Null)))
    M3741 = R
    Exit Function
EH:
    M3741 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3742() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "*"))
    M3742 = R
    Exit Function
EH:
    M3742 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3743() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "*"))
    M3743 = R
    Exit Function
EH:
    M3743 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3744() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "*"))
    M3744 = R
    Exit Function
EH:
    M3744 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3745() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "*"))
    M3745 = R
    Exit Function
EH:
    M3745 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3746() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "*"))
    M3746 = R
    Exit Function
EH:
    M3746 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3747() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "*"))
    M3747 = R
    Exit Function
EH:
    M3747 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3748() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "*"))
    M3748 = R
    Exit Function
EH:
    M3748 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3749() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "*"))
    M3749 = R
    Exit Function
EH:
    M3749 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3750() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "*"))
    M3750 = R
    Exit Function
EH:
    M3750 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3751() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "*"))
    M3751 = R
    Exit Function
EH:
    M3751 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3752() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "*"))
    M3752 = R
    Exit Function
EH:
    M3752 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3753() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "*"))
    M3753 = R
    Exit Function
EH:
    M3753 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3754() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "*"))
    M3754 = R
    Exit Function
EH:
    M3754 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3755() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "*"))
    M3755 = R
    Exit Function
EH:
    M3755 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3756() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "*"))
    M3756 = R
    Exit Function
EH:
    M3756 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3757() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "*"))
    M3757 = R
    Exit Function
EH:
    M3757 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3758() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "*"))
    M3758 = R
    Exit Function
EH:
    M3758 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3759() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "*"))
    M3759 = R
    Exit Function
EH:
    M3759 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3760() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "*"))
    M3760 = R
    Exit Function
EH:
    M3760 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3761() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "*"))
    M3761 = R
    Exit Function
EH:
    M3761 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3762() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "?*"))
    M3762 = R
    Exit Function
EH:
    M3762 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3763() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "?*"))
    M3763 = R
    Exit Function
EH:
    M3763 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3764() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "?*"))
    M3764 = R
    Exit Function
EH:
    M3764 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3765() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "?*"))
    M3765 = R
    Exit Function
EH:
    M3765 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3766() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "?*"))
    M3766 = R
    Exit Function
EH:
    M3766 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3767() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "?*"))
    M3767 = R
    Exit Function
EH:
    M3767 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3768() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "?*"))
    M3768 = R
    Exit Function
EH:
    M3768 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3769() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "?*"))
    M3769 = R
    Exit Function
EH:
    M3769 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3770() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "?*"))
    M3770 = R
    Exit Function
EH:
    M3770 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3771() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "?*"))
    M3771 = R
    Exit Function
EH:
    M3771 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3772() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "?*"))
    M3772 = R
    Exit Function
EH:
    M3772 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3773() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "?*"))
    M3773 = R
    Exit Function
EH:
    M3773 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3774() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "?*"))
    M3774 = R
    Exit Function
EH:
    M3774 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3775() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "?*"))
    M3775 = R
    Exit Function
EH:
    M3775 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3776() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "?*"))
    M3776 = R
    Exit Function
EH:
    M3776 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3777() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "?*"))
    M3777 = R
    Exit Function
EH:
    M3777 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3778() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "?*"))
    M3778 = R
    Exit Function
EH:
    M3778 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3779() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "?*"))
    M3779 = R
    Exit Function
EH:
    M3779 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3780() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "?*"))
    M3780 = R
    Exit Function
EH:
    M3780 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3781() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "?*"))
    M3781 = R
    Exit Function
EH:
    M3781 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3782() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "[a-c]*"))
    M3782 = R
    Exit Function
EH:
    M3782 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3783() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "[a-c]*"))
    M3783 = R
    Exit Function
EH:
    M3783 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3784() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "[a-c]*"))
    M3784 = R
    Exit Function
EH:
    M3784 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3785() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "[a-c]*"))
    M3785 = R
    Exit Function
EH:
    M3785 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3786() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "[a-c]*"))
    M3786 = R
    Exit Function
EH:
    M3786 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3787() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "[a-c]*"))
    M3787 = R
    Exit Function
EH:
    M3787 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3788() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "[a-c]*"))
    M3788 = R
    Exit Function
EH:
    M3788 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3789() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "[a-c]*"))
    M3789 = R
    Exit Function
EH:
    M3789 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3790() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "[a-c]*"))
    M3790 = R
    Exit Function
EH:
    M3790 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3791() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "[a-c]*"))
    M3791 = R
    Exit Function
EH:
    M3791 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3792() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "[a-c]*"))
    M3792 = R
    Exit Function
EH:
    M3792 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3793() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "[a-c]*"))
    M3793 = R
    Exit Function
EH:
    M3793 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3794() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "[a-c]*"))
    M3794 = R
    Exit Function
EH:
    M3794 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3795() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "[a-c]*"))
    M3795 = R
    Exit Function
EH:
    M3795 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3796() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "[a-c]*"))
    M3796 = R
    Exit Function
EH:
    M3796 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3797() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "[a-c]*"))
    M3797 = R
    Exit Function
EH:
    M3797 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3798() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "[a-c]*"))
    M3798 = R
    Exit Function
EH:
    M3798 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3799() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "[a-c]*"))
    M3799 = R
    Exit Function
EH:
    M3799 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3800() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "[a-c]*"))
    M3800 = R
    Exit Function
EH:
    M3800 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3801() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "[a-c]*"))
    M3801 = R
    Exit Function
EH:
    M3801 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3802() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "*[!a-z]*"))
    M3802 = R
    Exit Function
EH:
    M3802 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3803() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3803 = R
    Exit Function
EH:
    M3803 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3804() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3804 = R
    Exit Function
EH:
    M3804 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3805() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3805 = R
    Exit Function
EH:
    M3805 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3806() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "*[!a-z]*"))
    M3806 = R
    Exit Function
EH:
    M3806 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3807() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3807 = R
    Exit Function
EH:
    M3807 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3808() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3808 = R
    Exit Function
EH:
    M3808 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3809() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3809 = R
    Exit Function
EH:
    M3809 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3810() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3810 = R
    Exit Function
EH:
    M3810 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3811() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3811 = R
    Exit Function
EH:
    M3811 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3812() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "*[!a-z]*"))
    M3812 = R
    Exit Function
EH:
    M3812 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3813() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3813 = R
    Exit Function
EH:
    M3813 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3814() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3814 = R
    Exit Function
EH:
    M3814 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3815() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3815 = R
    Exit Function
EH:
    M3815 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3816() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "*[!a-z]*"))
    M3816 = R
    Exit Function
EH:
    M3816 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3817() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "*[!a-z]*"))
    M3817 = R
    Exit Function
EH:
    M3817 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3818() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3818 = R
    Exit Function
EH:
    M3818 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3819() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3819 = R
    Exit Function
EH:
    M3819 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3820() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3820 = R
    Exit Function
EH:
    M3820 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3821() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "*[!a-z]*"))
    M3821 = R
    Exit Function
EH:
    M3821 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3822() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "[[]*"))
    M3822 = R
    Exit Function
EH:
    M3822 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3823() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "[[]*"))
    M3823 = R
    Exit Function
EH:
    M3823 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3824() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "[[]*"))
    M3824 = R
    Exit Function
EH:
    M3824 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3825() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "[[]*"))
    M3825 = R
    Exit Function
EH:
    M3825 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3826() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "[[]*"))
    M3826 = R
    Exit Function
EH:
    M3826 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3827() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "[[]*"))
    M3827 = R
    Exit Function
EH:
    M3827 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3828() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "[[]*"))
    M3828 = R
    Exit Function
EH:
    M3828 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3829() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "[[]*"))
    M3829 = R
    Exit Function
EH:
    M3829 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3830() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "[[]*"))
    M3830 = R
    Exit Function
EH:
    M3830 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3831() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "[[]*"))
    M3831 = R
    Exit Function
EH:
    M3831 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3832() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "[[]*"))
    M3832 = R
    Exit Function
EH:
    M3832 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3833() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "[[]*"))
    M3833 = R
    Exit Function
EH:
    M3833 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3834() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "[[]*"))
    M3834 = R
    Exit Function
EH:
    M3834 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3835() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "[[]*"))
    M3835 = R
    Exit Function
EH:
    M3835 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3836() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "[[]*"))
    M3836 = R
    Exit Function
EH:
    M3836 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3837() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "[[]*"))
    M3837 = R
    Exit Function
EH:
    M3837 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3838() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "[[]*"))
    M3838 = R
    Exit Function
EH:
    M3838 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3839() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "[[]*"))
    M3839 = R
    Exit Function
EH:
    M3839 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3840() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "[[]*"))
    M3840 = R
    Exit Function
EH:
    M3840 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3841() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "[[]*"))
    M3841 = R
    Exit Function
EH:
    M3841 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3842() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(s Like "#*"))
    M3842 = R
    Exit Function
EH:
    M3842 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3843() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(s Like "#*"))
    M3843 = R
    Exit Function
EH:
    M3843 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3844() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(s Like "#*"))
    M3844 = R
    Exit Function
EH:
    M3844 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3845() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(s Like "#*"))
    M3845 = R
    Exit Function
EH:
    M3845 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3846() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(s Like "#*"))
    M3846 = R
    Exit Function
EH:
    M3846 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3847() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(s Like "#*"))
    M3847 = R
    Exit Function
EH:
    M3847 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3848() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(s Like "#*"))
    M3848 = R
    Exit Function
EH:
    M3848 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3849() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(s Like "#*"))
    M3849 = R
    Exit Function
EH:
    M3849 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3850() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(s Like "#*"))
    M3850 = R
    Exit Function
EH:
    M3850 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3851() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(s Like "#*"))
    M3851 = R
    Exit Function
EH:
    M3851 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3852() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(s Like "#*"))
    M3852 = R
    Exit Function
EH:
    M3852 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3853() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(s Like "#*"))
    M3853 = R
    Exit Function
EH:
    M3853 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3854() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(s Like "#*"))
    M3854 = R
    Exit Function
EH:
    M3854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3855() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(s Like "#*"))
    M3855 = R
    Exit Function
EH:
    M3855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3856() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(s Like "#*"))
    M3856 = R
    Exit Function
EH:
    M3856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3857() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(s Like "#*"))
    M3857 = R
    Exit Function
EH:
    M3857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3858() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(s Like "#*"))
    M3858 = R
    Exit Function
EH:
    M3858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3859() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(s Like "#*"))
    M3859 = R
    Exit Function
EH:
    M3859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3860() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(s Like "#*"))
    M3860 = R
    Exit Function
EH:
    M3860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3861() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(s Like "#*"))
    M3861 = R
    Exit Function
EH:
    M3861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3862() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3862 = R
    Exit Function
EH:
    M3862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3863() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3863 = R
    Exit Function
EH:
    M3863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3864() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3864 = R
    Exit Function
EH:
    M3864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3865() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3865 = R
    Exit Function
EH:
    M3865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3866() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3866 = R
    Exit Function
EH:
    M3866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3867() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3867 = R
    Exit Function
EH:
    M3867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3868() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3868 = R
    Exit Function
EH:
    M3868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3869() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3869 = R
    Exit Function
EH:
    M3869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3870() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3870 = R
    Exit Function
EH:
    M3870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3871() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3871 = R
    Exit Function
EH:
    M3871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3872() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3872 = R
    Exit Function
EH:
    M3872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3873() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3873 = R
    Exit Function
EH:
    M3873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3874() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3874 = R
    Exit Function
EH:
    M3874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3875() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3875 = R
    Exit Function
EH:
    M3875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3876() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3876 = R
    Exit Function
EH:
    M3876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3877() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3877 = R
    Exit Function
EH:
    M3877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3878() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3878 = R
    Exit Function
EH:
    M3878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3879() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3879 = R
    Exit Function
EH:
    M3879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3880() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3880 = R
    Exit Function
EH:
    M3880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3881() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Left(s, 1) Like "[A-Z]"))
    M3881 = R
    Exit Function
EH:
    M3881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3882() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV((s = LCase(s))))
    M3882 = R
    Exit Function
EH:
    M3882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3883() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV((s = LCase(s))))
    M3883 = R
    Exit Function
EH:
    M3883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3884() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV((s = LCase(s))))
    M3884 = R
    Exit Function
EH:
    M3884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3885() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV((s = LCase(s))))
    M3885 = R
    Exit Function
EH:
    M3885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3886() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV((s = LCase(s))))
    M3886 = R
    Exit Function
EH:
    M3886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3887() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV((s = LCase(s))))
    M3887 = R
    Exit Function
EH:
    M3887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3888() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV((s = LCase(s))))
    M3888 = R
    Exit Function
EH:
    M3888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3889() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV((s = LCase(s))))
    M3889 = R
    Exit Function
EH:
    M3889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3890() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV((s = LCase(s))))
    M3890 = R
    Exit Function
EH:
    M3890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3891() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV((s = LCase(s))))
    M3891 = R
    Exit Function
EH:
    M3891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3892() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV((s = LCase(s))))
    M3892 = R
    Exit Function
EH:
    M3892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3893() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV((s = LCase(s))))
    M3893 = R
    Exit Function
EH:
    M3893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3894() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV((s = LCase(s))))
    M3894 = R
    Exit Function
EH:
    M3894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3895() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV((s = LCase(s))))
    M3895 = R
    Exit Function
EH:
    M3895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3896() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV((s = LCase(s))))
    M3896 = R
    Exit Function
EH:
    M3896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3897() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV((s = LCase(s))))
    M3897 = R
    Exit Function
EH:
    M3897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3898() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV((s = LCase(s))))
    M3898 = R
    Exit Function
EH:
    M3898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3899() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV((s = LCase(s))))
    M3899 = R
    Exit Function
EH:
    M3899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3900() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV((s = LCase(s))))
    M3900 = R
    Exit Function
EH:
    M3900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3901() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV((s = LCase(s))))
    M3901 = R
    Exit Function
EH:
    M3901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3902() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV((s < "b")))
    M3902 = R
    Exit Function
EH:
    M3902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3903() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV((s < "b")))
    M3903 = R
    Exit Function
EH:
    M3903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3904() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV((s < "b")))
    M3904 = R
    Exit Function
EH:
    M3904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3905() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV((s < "b")))
    M3905 = R
    Exit Function
EH:
    M3905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3906() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV((s < "b")))
    M3906 = R
    Exit Function
EH:
    M3906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3907() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV((s < "b")))
    M3907 = R
    Exit Function
EH:
    M3907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3908() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV((s < "b")))
    M3908 = R
    Exit Function
EH:
    M3908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3909() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV((s < "b")))
    M3909 = R
    Exit Function
EH:
    M3909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3910() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV((s < "b")))
    M3910 = R
    Exit Function
EH:
    M3910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3911() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV((s < "b")))
    M3911 = R
    Exit Function
EH:
    M3911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3912() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV((s < "b")))
    M3912 = R
    Exit Function
EH:
    M3912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3913() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV((s < "b")))
    M3913 = R
    Exit Function
EH:
    M3913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3914() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV((s < "b")))
    M3914 = R
    Exit Function
EH:
    M3914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3915() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV((s < "b")))
    M3915 = R
    Exit Function
EH:
    M3915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3916() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV((s < "b")))
    M3916 = R
    Exit Function
EH:
    M3916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3917() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV((s < "b")))
    M3917 = R
    Exit Function
EH:
    M3917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3918() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV((s < "b")))
    M3918 = R
    Exit Function
EH:
    M3918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3919() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV((s < "b")))
    M3919 = R
    Exit Function
EH:
    M3919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3920() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV((s < "b")))
    M3920 = R
    Exit Function
EH:
    M3920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3921() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV((s < "b")))
    M3921 = R
    Exit Function
EH:
    M3921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3922() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV((s > "")))
    M3922 = R
    Exit Function
EH:
    M3922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3923() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV((s > "")))
    M3923 = R
    Exit Function
EH:
    M3923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3924() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV((s > "")))
    M3924 = R
    Exit Function
EH:
    M3924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3925() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV((s > "")))
    M3925 = R
    Exit Function
EH:
    M3925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3926() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV((s > "")))
    M3926 = R
    Exit Function
EH:
    M3926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3927() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV((s > "")))
    M3927 = R
    Exit Function
EH:
    M3927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3928() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV((s > "")))
    M3928 = R
    Exit Function
EH:
    M3928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3929() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV((s > "")))
    M3929 = R
    Exit Function
EH:
    M3929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3930() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV((s > "")))
    M3930 = R
    Exit Function
EH:
    M3930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3931() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV((s > "")))
    M3931 = R
    Exit Function
EH:
    M3931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3932() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV((s > "")))
    M3932 = R
    Exit Function
EH:
    M3932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3933() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV((s > "")))
    M3933 = R
    Exit Function
EH:
    M3933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3934() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV((s > "")))
    M3934 = R
    Exit Function
EH:
    M3934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3935() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV((s > "")))
    M3935 = R
    Exit Function
EH:
    M3935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3936() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV((s > "")))
    M3936 = R
    Exit Function
EH:
    M3936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3937() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV((s > "")))
    M3937 = R
    Exit Function
EH:
    M3937 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3938() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV((s > "")))
    M3938 = R
    Exit Function
EH:
    M3938 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3939() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV((s > "")))
    M3939 = R
    Exit Function
EH:
    M3939 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3940() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV((s > "")))
    M3940 = R
    Exit Function
EH:
    M3940 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3941() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV((s > "")))
    M3941 = R
    Exit Function
EH:
    M3941 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3942() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(UCase(Null))) & Dump(fV(Left(Null, 1))) & Dump(fV(Mid(Null, 1))) & Dump(fV(Trim(Null))) & Dump(fV(Replace(Null, "a", "b")))
    M3942 = R
    Exit Function
EH:
    M3942 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3943() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Replace("abc", Null, "b")
    M3943 = R
    Exit Function
EH:
    M3943 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3944() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Replace("abc", "a", Null)
    M3944 = R
    Exit Function
EH:
    M3944 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3945() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Split(Null)
    M3945 = R
    Exit Function
EH:
    M3945 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3946() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(InStr(Null, "a"))) & Dump(fV(InStr("a", Null))) & Dump(fV(InStrRev(Null, "a")))
    M3946 = R
    Exit Function
EH:
    M3946 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3947() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(StrConv(Null, 1))) & Dump(fV(StrComp(Null, "a")))
    M3947 = R
    Exit Function
EH:
    M3947 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3948() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Null)
    M3948 = R
    Exit Function
EH:
    M3948 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3949() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array(1, Null, "a"), ",")
    M3949 = R
    Exit Function
EH:
    M3949 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3950() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array(1, 2), Null)
    M3950 = R
    Exit Function
EH:
    M3950 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3951() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array(), ",") & "|" & Join(Split(""), ",") & "|" & UBound(Split("")) & "|" & LBound(Split(""))
    M3951 = R
    Exit Function
EH:
    M3951 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3952() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a b c")))
    M3952 = R
    Exit Function
EH:
    M3952 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3953() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a--b--c", "--", 2)))
    M3953 = R
    Exit Function
EH:
    M3953 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3954() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a", ",", 0)))
    M3954 = R
    Exit Function
EH:
    M3954 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3955() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a,b", ",", -2)))
    M3955 = R
    Exit Function
EH:
    M3955 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3956() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("aXbxc", "x", -1, vbTextCompare)))
    M3956 = R
    Exit Function
EH:
    M3956 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3957() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("aXbxc", "x", -1, vbBinaryCompare)))
    M3957 = R
    Exit Function
EH:
    M3957 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3958() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("aXbxc", "x", -1, 5)))
    M3958 = R
    Exit Function
EH:
    M3958 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3959() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a,b", ",", , 2)))
    M3959 = R
    Exit Function
EH:
    M3959 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3960() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("abc", "")))
    M3960 = R
    Exit Function
EH:
    M3960 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3961() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("", "")))
    M3961 = R
    Exit Function
EH:
    M3961 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3962() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split(" a  b ", " ")))
    M3962 = R
    Exit Function
EH:
    M3962 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3963() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Split("a" & vbCrLf & "b", vbLf)))
    M3963 = R
    Exit Function
EH:
    M3963 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3964() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Dump(fV(Join(Array("a", "b"), "")))
    M3964 = R
    Exit Function
EH:
    M3964 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3965() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array("a", "b"), 5)
    M3965 = R
    Exit Function
EH:
    M3965 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3966() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join("a")
    M3966 = R
    Exit Function
EH:
    M3966 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3967() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(5)
    M3967 = R
    Exit Function
EH:
    M3967 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3968() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array(Array(1), 2))
    M3968 = R
    Exit Function
EH:
    M3968 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3969() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Join(Array(Empty, Empty), "-")
    M3969 = R
    Exit Function
EH:
    M3969 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3970() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStrRev("abcabc", "b") & InStrRev("abcabc", "b", 3) & InStrRev("abcabc", "b", 2) & InStrRev("abcabc", "bc", 6) & InStrRev("abcabc", "bc", 5) & InStrRev("abcabc", "abc", 3) & InStrRev("abcabc", "abc", -1)
    M3970 = R
    Exit Function
EH:
    M3970 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3971() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStrRev("abcabc", "B") & InStrRev("abcabc", "B", , 1) & InStrRev("abcabc", "B", -1, vbTextCompare) & InStrRev("", "") & InStrRev("a", "") & InStrRev("", "a") & InStrRev("abc", "", 2)
    M3971 = R
    Exit Function
EH:
    M3971 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3972() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStrRev("abc", "a", -2)
    M3972 = R
    Exit Function
EH:
    M3972 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3973() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStrRev("abc", "a", 0)
    M3973 = R
    Exit Function
EH:
    M3973 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3974() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStrRev("abc", "a", , 5)
    M3974 = R
    Exit Function
EH:
    M3974 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3975() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(0, "abc", "a")
    M3975 = R
    Exit Function
EH:
    M3975 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3976() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(-1, "abc", "a")
    M3976 = R
    Exit Function
EH:
    M3976 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3977() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(4, "abc", "")
    M3977 = R
    Exit Function
EH:
    M3977 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3978() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(3, "abc", "")
    M3978 = R
    Exit Function
EH:
    M3978 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3979() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(2, "abc", "")
    M3979 = R
    Exit Function
EH:
    M3979 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3980() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(1, "", "")
    M3980 = R
    Exit Function
EH:
    M3980 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3981() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(1, "abc", "B", 2)
    M3981 = R
    Exit Function
EH:
    M3981 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3982() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr("abc", "B", 1)
    M3982 = R
    Exit Function
EH:
    M3982 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3983() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(1, "aXb", "x", vbTextCompare) & InStr("abc", "c", vbBinaryCompare)
    M3983 = R
    Exit Function
EH:
    M3983 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3984() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(1, "abc", "c", 5)
    M3984 = R
    Exit Function
EH:
    M3984 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3985() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(2147483647, "abc", "c")
    M3985 = R
    Exit Function
EH:
    M3985 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3986() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = InStr(2147483648#, "abc", "c")
    M3986 = R
    Exit Function
EH:
    M3986 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3987() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Replace("aaa", "a", "b", 2, 2) & "|" & Replace("aaa", "a", "b", 4) & "|" & Replace("aaa", "a", "b", 3) & "|" & Replace("aaa", "aa", "b") & "|" & Replace("aaaa", "aa", "b") & "|" & Replace("abc", "abc", "") & "|" & Replace("", "a", "b") & "|" & Replace("", "", "b")
    M3987 = R
    Exit Function
EH:
    M3987 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3988() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Replace("abc", "B", "x", 1, -1, vbTextCompare) & Replace("abc", "B", "x") & Replace("ABC", "b", "xx", 2, 1, 1) & Replace("abc", "c", "x", -1)
    M3988 = R
    Exit Function
EH:
    M3988 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3989() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = Replace("abc", "c", "x", 4) & "|" & Replace("abc", "c", "x", 3, 0) & "|" & Replace("abc", "c", "x", 3, 1) & "|" & Replace("abc", "c", "x", 1, 0)
    M3989 = R
    Exit Function
EH:
    M3989 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3990() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc def", vbProperCase) & "|" & StrConv("ABC dEF-ghi_jkl 1m", vbProperCase) & "|" & StrConv("oneil mcdonald", 3)
    M3990 = R
    Exit Function
EH:
    M3990 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3991() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", vbWide) & StrConv("ABC", vbLowerCase + vbNarrow)
    M3991 = R
    Exit Function
EH:
    M3991 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3992() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 256)
    M3992 = R
    Exit Function
EH:
    M3992 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3993() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 1 + 2)
    M3993 = R
    Exit Function
EH:
    M3993 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3994() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 4)
    M3994 = R
    Exit Function
EH:
    M3994 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3995() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 8)
    M3995 = R
    Exit Function
EH:
    M3995 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3996() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 16)
    M3996 = R
    Exit Function
EH:
    M3996 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3997() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 1024)
    M3997 = R
    Exit Function
EH:
    M3997 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3998() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", -1)
    M3998 = R
    Exit Function
EH:
    M3998 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M3999() As Variant
    On Error GoTo EH
    Dim R As Variant
    R = StrConv("abc", 32768)
    M3999 = R
    Exit Function
EH:
    M3999 = "#ERR " & Err.Number & " " & Err.Description
End Function
