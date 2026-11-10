Attribute VB_Name = "VbaLang3_2"
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

Public Function LangThree2(ByVal k As Long) As Variant
    Select Case (k - 2000) \ 100
    Case 0: LangThree2 = MC0(k)
    Case 1: LangThree2 = MC1(k)
    Case 2: LangThree2 = MC2(k)
    Case 3: LangThree2 = MC3(k)
    Case 4: LangThree2 = MC4(k)
    Case 5: LangThree2 = MC5(k)
    Case 6: LangThree2 = MC6(k)
    Case 7: LangThree2 = MC7(k)
    Case 8: LangThree2 = MC8(k)
    Case 9: LangThree2 = MC9(k)
    End Select
End Function

Private Function MC0(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2000: MC0 = M2000()
    Case 2001: MC0 = M2001()
    Case 2002: MC0 = M2002()
    Case 2003: MC0 = M2003()
    Case 2004: MC0 = M2004()
    Case 2005: MC0 = M2005()
    Case 2006: MC0 = M2006()
    Case 2007: MC0 = M2007()
    Case 2008: MC0 = M2008()
    Case 2009: MC0 = M2009()
    Case 2010: MC0 = M2010()
    Case 2011: MC0 = M2011()
    Case 2012: MC0 = M2012()
    Case 2013: MC0 = M2013()
    Case 2014: MC0 = M2014()
    Case 2015: MC0 = M2015()
    Case 2016: MC0 = M2016()
    Case 2017: MC0 = M2017()
    Case 2018: MC0 = M2018()
    Case 2019: MC0 = M2019()
    Case 2020: MC0 = M2020()
    Case 2021: MC0 = M2021()
    Case 2022: MC0 = M2022()
    Case 2023: MC0 = M2023()
    Case 2024: MC0 = M2024()
    Case 2025: MC0 = M2025()
    Case 2026: MC0 = M2026()
    Case 2027: MC0 = M2027()
    Case 2028: MC0 = M2028()
    Case 2029: MC0 = M2029()
    Case 2030: MC0 = M2030()
    Case 2031: MC0 = M2031()
    Case 2032: MC0 = M2032()
    Case 2033: MC0 = M2033()
    Case 2034: MC0 = M2034()
    Case 2035: MC0 = M2035()
    Case 2036: MC0 = M2036()
    Case 2037: MC0 = M2037()
    Case 2038: MC0 = M2038()
    Case 2039: MC0 = M2039()
    Case 2040: MC0 = M2040()
    Case 2041: MC0 = M2041()
    Case 2042: MC0 = M2042()
    Case 2043: MC0 = M2043()
    Case 2044: MC0 = M2044()
    Case 2045: MC0 = M2045()
    Case 2046: MC0 = M2046()
    Case 2047: MC0 = M2047()
    Case 2048: MC0 = M2048()
    Case 2049: MC0 = M2049()
    Case 2050: MC0 = M2050()
    Case 2051: MC0 = M2051()
    Case 2052: MC0 = M2052()
    Case 2053: MC0 = M2053()
    Case 2054: MC0 = M2054()
    Case 2055: MC0 = M2055()
    Case 2056: MC0 = M2056()
    Case 2057: MC0 = M2057()
    Case 2058: MC0 = M2058()
    Case 2059: MC0 = M2059()
    Case 2060: MC0 = M2060()
    Case 2061: MC0 = M2061()
    Case 2062: MC0 = M2062()
    Case 2063: MC0 = M2063()
    Case 2064: MC0 = M2064()
    Case 2065: MC0 = M2065()
    Case 2066: MC0 = M2066()
    Case 2067: MC0 = M2067()
    Case 2068: MC0 = M2068()
    Case 2069: MC0 = M2069()
    Case 2070: MC0 = M2070()
    Case 2071: MC0 = M2071()
    Case 2072: MC0 = M2072()
    Case 2073: MC0 = M2073()
    Case 2074: MC0 = M2074()
    Case 2075: MC0 = M2075()
    Case 2076: MC0 = M2076()
    Case 2077: MC0 = M2077()
    Case 2078: MC0 = M2078()
    Case 2079: MC0 = M2079()
    Case 2080: MC0 = M2080()
    Case 2081: MC0 = M2081()
    Case 2082: MC0 = M2082()
    Case 2083: MC0 = M2083()
    Case 2084: MC0 = M2084()
    Case 2085: MC0 = M2085()
    Case 2086: MC0 = M2086()
    Case 2087: MC0 = M2087()
    Case 2088: MC0 = M2088()
    Case 2089: MC0 = M2089()
    Case 2090: MC0 = M2090()
    Case 2091: MC0 = M2091()
    Case 2092: MC0 = M2092()
    Case 2093: MC0 = M2093()
    Case 2094: MC0 = M2094()
    Case 2095: MC0 = M2095()
    Case 2096: MC0 = M2096()
    Case 2097: MC0 = M2097()
    Case 2098: MC0 = M2098()
    Case 2099: MC0 = M2099()
    End Select
    Exit Function
EH:
    MC0 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC1(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2100: MC1 = M2100()
    Case 2101: MC1 = M2101()
    Case 2102: MC1 = M2102()
    Case 2103: MC1 = M2103()
    Case 2104: MC1 = M2104()
    Case 2105: MC1 = M2105()
    Case 2106: MC1 = M2106()
    Case 2107: MC1 = M2107()
    Case 2108: MC1 = M2108()
    Case 2109: MC1 = M2109()
    Case 2110: MC1 = M2110()
    Case 2111: MC1 = M2111()
    Case 2112: MC1 = M2112()
    Case 2113: MC1 = M2113()
    Case 2114: MC1 = M2114()
    Case 2115: MC1 = M2115()
    Case 2116: MC1 = M2116()
    Case 2117: MC1 = M2117()
    Case 2118: MC1 = M2118()
    Case 2119: MC1 = M2119()
    Case 2120: MC1 = M2120()
    Case 2121: MC1 = M2121()
    Case 2122: MC1 = M2122()
    Case 2123: MC1 = M2123()
    Case 2124: MC1 = M2124()
    Case 2125: MC1 = M2125()
    Case 2126: MC1 = M2126()
    Case 2127: MC1 = M2127()
    Case 2128: MC1 = M2128()
    Case 2129: MC1 = M2129()
    Case 2130: MC1 = M2130()
    Case 2131: MC1 = M2131()
    Case 2132: MC1 = M2132()
    Case 2133: MC1 = M2133()
    Case 2134: MC1 = M2134()
    Case 2135: MC1 = M2135()
    Case 2136: MC1 = M2136()
    Case 2137: MC1 = M2137()
    Case 2138: MC1 = M2138()
    Case 2139: MC1 = M2139()
    Case 2140: MC1 = M2140()
    Case 2141: MC1 = M2141()
    Case 2142: MC1 = M2142()
    Case 2143: MC1 = M2143()
    Case 2144: MC1 = M2144()
    Case 2145: MC1 = M2145()
    Case 2146: MC1 = M2146()
    Case 2147: MC1 = M2147()
    Case 2148: MC1 = M2148()
    Case 2149: MC1 = M2149()
    Case 2150: MC1 = M2150()
    Case 2151: MC1 = M2151()
    Case 2152: MC1 = M2152()
    Case 2153: MC1 = M2153()
    Case 2154: MC1 = M2154()
    Case 2155: MC1 = M2155()
    Case 2156: MC1 = M2156()
    Case 2157: MC1 = M2157()
    Case 2158: MC1 = M2158()
    Case 2159: MC1 = M2159()
    Case 2160: MC1 = M2160()
    Case 2161: MC1 = M2161()
    Case 2162: MC1 = M2162()
    Case 2163: MC1 = M2163()
    Case 2164: MC1 = M2164()
    Case 2165: MC1 = M2165()
    Case 2166: MC1 = M2166()
    Case 2167: MC1 = M2167()
    Case 2168: MC1 = M2168()
    Case 2169: MC1 = M2169()
    Case 2170: MC1 = M2170()
    Case 2171: MC1 = M2171()
    Case 2172: MC1 = M2172()
    Case 2173: MC1 = M2173()
    Case 2174: MC1 = M2174()
    Case 2175: MC1 = M2175()
    Case 2176: MC1 = M2176()
    Case 2177: MC1 = M2177()
    Case 2178: MC1 = M2178()
    Case 2179: MC1 = M2179()
    Case 2180: MC1 = M2180()
    Case 2181: MC1 = M2181()
    Case 2182: MC1 = M2182()
    Case 2183: MC1 = M2183()
    Case 2184: MC1 = M2184()
    Case 2185: MC1 = M2185()
    Case 2186: MC1 = M2186()
    Case 2187: MC1 = M2187()
    Case 2188: MC1 = M2188()
    Case 2189: MC1 = M2189()
    Case 2190: MC1 = M2190()
    Case 2191: MC1 = M2191()
    Case 2192: MC1 = M2192()
    Case 2193: MC1 = M2193()
    Case 2194: MC1 = M2194()
    Case 2195: MC1 = M2195()
    Case 2196: MC1 = M2196()
    Case 2197: MC1 = M2197()
    Case 2198: MC1 = M2198()
    Case 2199: MC1 = M2199()
    End Select
    Exit Function
EH:
    MC1 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC2(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2200: MC2 = M2200()
    Case 2201: MC2 = M2201()
    Case 2202: MC2 = M2202()
    Case 2203: MC2 = M2203()
    Case 2204: MC2 = M2204()
    Case 2205: MC2 = M2205()
    Case 2206: MC2 = M2206()
    Case 2207: MC2 = M2207()
    Case 2208: MC2 = M2208()
    Case 2209: MC2 = M2209()
    Case 2210: MC2 = M2210()
    Case 2211: MC2 = M2211()
    Case 2212: MC2 = M2212()
    Case 2213: MC2 = M2213()
    Case 2214: MC2 = M2214()
    Case 2215: MC2 = M2215()
    Case 2216: MC2 = M2216()
    Case 2217: MC2 = M2217()
    Case 2218: MC2 = M2218()
    Case 2219: MC2 = M2219()
    Case 2220: MC2 = M2220()
    Case 2221: MC2 = M2221()
    Case 2222: MC2 = M2222()
    Case 2223: MC2 = M2223()
    Case 2224: MC2 = M2224()
    Case 2225: MC2 = M2225()
    Case 2226: MC2 = M2226()
    Case 2227: MC2 = M2227()
    Case 2228: MC2 = M2228()
    Case 2229: MC2 = M2229()
    Case 2230: MC2 = M2230()
    Case 2231: MC2 = M2231()
    Case 2232: MC2 = M2232()
    Case 2233: MC2 = M2233()
    Case 2234: MC2 = M2234()
    Case 2235: MC2 = M2235()
    Case 2236: MC2 = M2236()
    Case 2237: MC2 = M2237()
    Case 2238: MC2 = M2238()
    Case 2239: MC2 = M2239()
    Case 2240: MC2 = M2240()
    Case 2241: MC2 = M2241()
    Case 2242: MC2 = M2242()
    Case 2243: MC2 = M2243()
    Case 2244: MC2 = M2244()
    Case 2245: MC2 = M2245()
    Case 2246: MC2 = M2246()
    Case 2247: MC2 = M2247()
    Case 2248: MC2 = M2248()
    Case 2249: MC2 = M2249()
    Case 2250: MC2 = M2250()
    Case 2251: MC2 = M2251()
    Case 2252: MC2 = M2252()
    Case 2253: MC2 = M2253()
    Case 2254: MC2 = M2254()
    Case 2255: MC2 = M2255()
    Case 2256: MC2 = M2256()
    Case 2257: MC2 = M2257()
    Case 2258: MC2 = M2258()
    Case 2259: MC2 = M2259()
    Case 2260: MC2 = M2260()
    Case 2261: MC2 = M2261()
    Case 2262: MC2 = M2262()
    Case 2263: MC2 = M2263()
    Case 2264: MC2 = M2264()
    Case 2265: MC2 = M2265()
    Case 2266: MC2 = M2266()
    Case 2267: MC2 = M2267()
    Case 2268: MC2 = M2268()
    Case 2269: MC2 = M2269()
    Case 2270: MC2 = M2270()
    Case 2271: MC2 = M2271()
    Case 2272: MC2 = M2272()
    Case 2273: MC2 = M2273()
    Case 2274: MC2 = M2274()
    Case 2275: MC2 = M2275()
    Case 2276: MC2 = M2276()
    Case 2277: MC2 = M2277()
    Case 2278: MC2 = M2278()
    Case 2279: MC2 = M2279()
    Case 2280: MC2 = M2280()
    Case 2281: MC2 = M2281()
    Case 2282: MC2 = M2282()
    Case 2283: MC2 = M2283()
    Case 2284: MC2 = M2284()
    Case 2285: MC2 = M2285()
    Case 2286: MC2 = M2286()
    Case 2287: MC2 = M2287()
    Case 2288: MC2 = M2288()
    Case 2289: MC2 = M2289()
    Case 2290: MC2 = M2290()
    Case 2291: MC2 = M2291()
    Case 2292: MC2 = M2292()
    Case 2293: MC2 = M2293()
    Case 2294: MC2 = M2294()
    Case 2295: MC2 = M2295()
    Case 2296: MC2 = M2296()
    Case 2297: MC2 = M2297()
    Case 2298: MC2 = M2298()
    Case 2299: MC2 = M2299()
    End Select
    Exit Function
EH:
    MC2 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC3(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2300: MC3 = M2300()
    Case 2301: MC3 = M2301()
    Case 2302: MC3 = M2302()
    Case 2303: MC3 = M2303()
    Case 2304: MC3 = M2304()
    Case 2305: MC3 = M2305()
    Case 2306: MC3 = M2306()
    Case 2307: MC3 = M2307()
    Case 2308: MC3 = M2308()
    Case 2309: MC3 = M2309()
    Case 2310: MC3 = M2310()
    Case 2311: MC3 = M2311()
    Case 2312: MC3 = M2312()
    Case 2313: MC3 = M2313()
    Case 2314: MC3 = M2314()
    Case 2315: MC3 = M2315()
    Case 2316: MC3 = M2316()
    Case 2317: MC3 = M2317()
    Case 2318: MC3 = M2318()
    Case 2319: MC3 = M2319()
    Case 2320: MC3 = M2320()
    Case 2321: MC3 = M2321()
    Case 2322: MC3 = M2322()
    Case 2323: MC3 = M2323()
    Case 2324: MC3 = M2324()
    Case 2325: MC3 = M2325()
    Case 2326: MC3 = M2326()
    Case 2327: MC3 = M2327()
    Case 2328: MC3 = M2328()
    Case 2329: MC3 = M2329()
    Case 2330: MC3 = M2330()
    Case 2331: MC3 = M2331()
    Case 2332: MC3 = M2332()
    Case 2333: MC3 = M2333()
    Case 2334: MC3 = M2334()
    Case 2335: MC3 = M2335()
    Case 2336: MC3 = M2336()
    Case 2337: MC3 = M2337()
    Case 2338: MC3 = M2338()
    Case 2339: MC3 = M2339()
    Case 2340: MC3 = M2340()
    Case 2341: MC3 = M2341()
    Case 2342: MC3 = M2342()
    Case 2343: MC3 = M2343()
    Case 2344: MC3 = M2344()
    Case 2345: MC3 = M2345()
    Case 2346: MC3 = M2346()
    Case 2347: MC3 = M2347()
    Case 2348: MC3 = M2348()
    Case 2349: MC3 = M2349()
    Case 2350: MC3 = M2350()
    Case 2351: MC3 = M2351()
    Case 2352: MC3 = M2352()
    Case 2353: MC3 = M2353()
    Case 2354: MC3 = M2354()
    Case 2355: MC3 = M2355()
    Case 2356: MC3 = M2356()
    Case 2357: MC3 = M2357()
    Case 2358: MC3 = M2358()
    Case 2359: MC3 = M2359()
    Case 2360: MC3 = M2360()
    Case 2361: MC3 = M2361()
    Case 2362: MC3 = M2362()
    Case 2363: MC3 = M2363()
    Case 2364: MC3 = M2364()
    Case 2365: MC3 = M2365()
    Case 2366: MC3 = M2366()
    Case 2367: MC3 = M2367()
    Case 2368: MC3 = M2368()
    Case 2369: MC3 = M2369()
    Case 2370: MC3 = M2370()
    Case 2371: MC3 = M2371()
    Case 2372: MC3 = M2372()
    Case 2373: MC3 = M2373()
    Case 2374: MC3 = M2374()
    Case 2375: MC3 = M2375()
    Case 2376: MC3 = M2376()
    Case 2377: MC3 = M2377()
    Case 2378: MC3 = M2378()
    Case 2379: MC3 = M2379()
    Case 2380: MC3 = M2380()
    Case 2381: MC3 = M2381()
    Case 2382: MC3 = M2382()
    Case 2383: MC3 = M2383()
    Case 2384: MC3 = M2384()
    Case 2385: MC3 = M2385()
    Case 2386: MC3 = M2386()
    Case 2387: MC3 = M2387()
    Case 2388: MC3 = M2388()
    Case 2389: MC3 = M2389()
    Case 2390: MC3 = M2390()
    Case 2391: MC3 = M2391()
    Case 2392: MC3 = M2392()
    Case 2393: MC3 = M2393()
    Case 2394: MC3 = M2394()
    Case 2395: MC3 = M2395()
    Case 2396: MC3 = M2396()
    Case 2397: MC3 = M2397()
    Case 2398: MC3 = M2398()
    Case 2399: MC3 = M2399()
    End Select
    Exit Function
EH:
    MC3 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC4(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2400: MC4 = M2400()
    Case 2401: MC4 = M2401()
    Case 2402: MC4 = M2402()
    Case 2403: MC4 = M2403()
    Case 2404: MC4 = M2404()
    Case 2405: MC4 = M2405()
    Case 2406: MC4 = M2406()
    Case 2407: MC4 = M2407()
    Case 2408: MC4 = M2408()
    Case 2409: MC4 = M2409()
    Case 2410: MC4 = M2410()
    Case 2411: MC4 = M2411()
    Case 2412: MC4 = M2412()
    Case 2413: MC4 = M2413()
    Case 2414: MC4 = M2414()
    Case 2415: MC4 = M2415()
    Case 2416: MC4 = M2416()
    Case 2417: MC4 = M2417()
    Case 2418: MC4 = M2418()
    Case 2419: MC4 = M2419()
    Case 2420: MC4 = M2420()
    Case 2421: MC4 = M2421()
    Case 2422: MC4 = M2422()
    Case 2423: MC4 = M2423()
    Case 2424: MC4 = M2424()
    Case 2425: MC4 = M2425()
    Case 2426: MC4 = M2426()
    Case 2427: MC4 = M2427()
    Case 2428: MC4 = M2428()
    Case 2429: MC4 = M2429()
    Case 2430: MC4 = M2430()
    Case 2431: MC4 = M2431()
    Case 2432: MC4 = M2432()
    Case 2433: MC4 = M2433()
    Case 2434: MC4 = M2434()
    Case 2435: MC4 = M2435()
    Case 2436: MC4 = M2436()
    Case 2437: MC4 = M2437()
    Case 2438: MC4 = M2438()
    Case 2439: MC4 = M2439()
    Case 2440: MC4 = M2440()
    Case 2441: MC4 = M2441()
    Case 2442: MC4 = M2442()
    Case 2443: MC4 = M2443()
    Case 2444: MC4 = M2444()
    Case 2445: MC4 = M2445()
    Case 2446: MC4 = M2446()
    Case 2447: MC4 = M2447()
    Case 2448: MC4 = M2448()
    Case 2449: MC4 = M2449()
    Case 2450: MC4 = M2450()
    Case 2451: MC4 = M2451()
    Case 2452: MC4 = M2452()
    Case 2453: MC4 = M2453()
    Case 2454: MC4 = M2454()
    Case 2455: MC4 = M2455()
    Case 2456: MC4 = M2456()
    Case 2457: MC4 = M2457()
    Case 2458: MC4 = M2458()
    Case 2459: MC4 = M2459()
    Case 2460: MC4 = M2460()
    Case 2461: MC4 = M2461()
    Case 2462: MC4 = M2462()
    Case 2463: MC4 = M2463()
    Case 2464: MC4 = M2464()
    Case 2465: MC4 = M2465()
    Case 2466: MC4 = M2466()
    Case 2467: MC4 = M2467()
    Case 2468: MC4 = M2468()
    Case 2469: MC4 = M2469()
    Case 2470: MC4 = M2470()
    Case 2471: MC4 = M2471()
    Case 2472: MC4 = M2472()
    Case 2473: MC4 = M2473()
    Case 2474: MC4 = M2474()
    Case 2475: MC4 = M2475()
    Case 2476: MC4 = M2476()
    Case 2477: MC4 = M2477()
    Case 2478: MC4 = M2478()
    Case 2479: MC4 = M2479()
    Case 2480: MC4 = M2480()
    Case 2481: MC4 = M2481()
    Case 2482: MC4 = M2482()
    Case 2483: MC4 = M2483()
    Case 2484: MC4 = M2484()
    Case 2485: MC4 = M2485()
    Case 2486: MC4 = M2486()
    Case 2487: MC4 = M2487()
    Case 2488: MC4 = M2488()
    Case 2489: MC4 = M2489()
    Case 2490: MC4 = M2490()
    Case 2491: MC4 = M2491()
    Case 2492: MC4 = M2492()
    Case 2493: MC4 = M2493()
    Case 2494: MC4 = M2494()
    Case 2495: MC4 = M2495()
    Case 2496: MC4 = M2496()
    Case 2497: MC4 = M2497()
    Case 2498: MC4 = M2498()
    Case 2499: MC4 = M2499()
    End Select
    Exit Function
EH:
    MC4 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC5(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2500: MC5 = M2500()
    Case 2501: MC5 = M2501()
    Case 2502: MC5 = M2502()
    Case 2503: MC5 = M2503()
    Case 2504: MC5 = M2504()
    Case 2505: MC5 = M2505()
    Case 2506: MC5 = M2506()
    Case 2507: MC5 = M2507()
    Case 2508: MC5 = M2508()
    Case 2509: MC5 = M2509()
    Case 2510: MC5 = M2510()
    Case 2511: MC5 = M2511()
    Case 2512: MC5 = M2512()
    Case 2513: MC5 = M2513()
    Case 2514: MC5 = M2514()
    Case 2515: MC5 = M2515()
    Case 2516: MC5 = M2516()
    Case 2517: MC5 = M2517()
    Case 2518: MC5 = M2518()
    Case 2519: MC5 = M2519()
    Case 2520: MC5 = M2520()
    Case 2521: MC5 = M2521()
    Case 2522: MC5 = M2522()
    Case 2523: MC5 = M2523()
    Case 2524: MC5 = M2524()
    Case 2525: MC5 = M2525()
    Case 2526: MC5 = M2526()
    Case 2527: MC5 = M2527()
    Case 2528: MC5 = M2528()
    Case 2529: MC5 = M2529()
    Case 2530: MC5 = M2530()
    Case 2531: MC5 = M2531()
    Case 2532: MC5 = M2532()
    Case 2533: MC5 = M2533()
    Case 2534: MC5 = M2534()
    Case 2535: MC5 = M2535()
    Case 2536: MC5 = M2536()
    Case 2537: MC5 = M2537()
    Case 2538: MC5 = M2538()
    Case 2539: MC5 = M2539()
    Case 2540: MC5 = M2540()
    Case 2541: MC5 = M2541()
    Case 2542: MC5 = M2542()
    Case 2543: MC5 = M2543()
    Case 2544: MC5 = M2544()
    Case 2545: MC5 = M2545()
    Case 2546: MC5 = M2546()
    Case 2547: MC5 = M2547()
    Case 2548: MC5 = M2548()
    Case 2549: MC5 = M2549()
    Case 2550: MC5 = M2550()
    Case 2551: MC5 = M2551()
    Case 2552: MC5 = M2552()
    Case 2553: MC5 = M2553()
    Case 2554: MC5 = M2554()
    Case 2555: MC5 = M2555()
    Case 2556: MC5 = M2556()
    Case 2557: MC5 = M2557()
    Case 2558: MC5 = M2558()
    Case 2559: MC5 = M2559()
    Case 2560: MC5 = M2560()
    Case 2561: MC5 = M2561()
    Case 2562: MC5 = M2562()
    Case 2563: MC5 = M2563()
    Case 2564: MC5 = M2564()
    Case 2565: MC5 = M2565()
    Case 2566: MC5 = M2566()
    Case 2567: MC5 = M2567()
    Case 2568: MC5 = M2568()
    Case 2569: MC5 = M2569()
    Case 2570: MC5 = M2570()
    Case 2571: MC5 = M2571()
    Case 2572: MC5 = M2572()
    Case 2573: MC5 = M2573()
    Case 2574: MC5 = M2574()
    Case 2575: MC5 = M2575()
    Case 2576: MC5 = M2576()
    Case 2577: MC5 = M2577()
    Case 2578: MC5 = M2578()
    Case 2579: MC5 = M2579()
    Case 2580: MC5 = M2580()
    Case 2581: MC5 = M2581()
    Case 2582: MC5 = M2582()
    Case 2583: MC5 = M2583()
    Case 2584: MC5 = M2584()
    Case 2585: MC5 = M2585()
    Case 2586: MC5 = M2586()
    Case 2587: MC5 = M2587()
    Case 2588: MC5 = M2588()
    Case 2589: MC5 = M2589()
    Case 2590: MC5 = M2590()
    Case 2591: MC5 = M2591()
    Case 2592: MC5 = M2592()
    Case 2593: MC5 = M2593()
    Case 2594: MC5 = M2594()
    Case 2595: MC5 = M2595()
    Case 2596: MC5 = M2596()
    Case 2597: MC5 = M2597()
    Case 2598: MC5 = M2598()
    Case 2599: MC5 = M2599()
    End Select
    Exit Function
EH:
    MC5 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC6(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2600: MC6 = M2600()
    Case 2601: MC6 = M2601()
    Case 2602: MC6 = M2602()
    Case 2603: MC6 = M2603()
    Case 2604: MC6 = M2604()
    Case 2605: MC6 = M2605()
    Case 2606: MC6 = M2606()
    Case 2607: MC6 = M2607()
    Case 2608: MC6 = M2608()
    Case 2609: MC6 = M2609()
    Case 2610: MC6 = M2610()
    Case 2611: MC6 = M2611()
    Case 2612: MC6 = M2612()
    Case 2613: MC6 = M2613()
    Case 2614: MC6 = M2614()
    Case 2615: MC6 = M2615()
    Case 2616: MC6 = M2616()
    Case 2617: MC6 = M2617()
    Case 2618: MC6 = M2618()
    Case 2619: MC6 = M2619()
    Case 2620: MC6 = M2620()
    Case 2621: MC6 = M2621()
    Case 2622: MC6 = M2622()
    Case 2623: MC6 = M2623()
    Case 2624: MC6 = M2624()
    Case 2625: MC6 = M2625()
    Case 2626: MC6 = M2626()
    Case 2627: MC6 = M2627()
    Case 2628: MC6 = M2628()
    Case 2629: MC6 = M2629()
    Case 2630: MC6 = M2630()
    Case 2631: MC6 = M2631()
    Case 2632: MC6 = M2632()
    Case 2633: MC6 = M2633()
    Case 2634: MC6 = M2634()
    Case 2635: MC6 = M2635()
    Case 2636: MC6 = M2636()
    Case 2637: MC6 = M2637()
    Case 2638: MC6 = M2638()
    Case 2639: MC6 = M2639()
    Case 2640: MC6 = M2640()
    Case 2641: MC6 = M2641()
    Case 2642: MC6 = M2642()
    Case 2643: MC6 = M2643()
    Case 2644: MC6 = M2644()
    Case 2645: MC6 = M2645()
    Case 2646: MC6 = M2646()
    Case 2647: MC6 = M2647()
    Case 2648: MC6 = M2648()
    Case 2649: MC6 = M2649()
    Case 2650: MC6 = M2650()
    Case 2651: MC6 = M2651()
    Case 2652: MC6 = M2652()
    Case 2653: MC6 = M2653()
    Case 2654: MC6 = M2654()
    Case 2655: MC6 = M2655()
    Case 2656: MC6 = M2656()
    Case 2657: MC6 = M2657()
    Case 2658: MC6 = M2658()
    Case 2659: MC6 = M2659()
    Case 2660: MC6 = M2660()
    Case 2661: MC6 = M2661()
    Case 2662: MC6 = M2662()
    Case 2663: MC6 = M2663()
    Case 2664: MC6 = M2664()
    Case 2665: MC6 = M2665()
    Case 2666: MC6 = M2666()
    Case 2667: MC6 = M2667()
    Case 2668: MC6 = M2668()
    Case 2669: MC6 = M2669()
    Case 2670: MC6 = M2670()
    Case 2671: MC6 = M2671()
    Case 2672: MC6 = M2672()
    Case 2673: MC6 = M2673()
    Case 2674: MC6 = M2674()
    Case 2675: MC6 = M2675()
    Case 2676: MC6 = M2676()
    Case 2677: MC6 = M2677()
    Case 2678: MC6 = M2678()
    Case 2679: MC6 = M2679()
    Case 2680: MC6 = M2680()
    Case 2681: MC6 = M2681()
    Case 2682: MC6 = M2682()
    Case 2683: MC6 = M2683()
    Case 2684: MC6 = M2684()
    Case 2685: MC6 = M2685()
    Case 2686: MC6 = M2686()
    Case 2687: MC6 = M2687()
    Case 2688: MC6 = M2688()
    Case 2689: MC6 = M2689()
    Case 2690: MC6 = M2690()
    Case 2691: MC6 = M2691()
    Case 2692: MC6 = M2692()
    Case 2693: MC6 = M2693()
    Case 2694: MC6 = M2694()
    Case 2695: MC6 = M2695()
    Case 2696: MC6 = M2696()
    Case 2697: MC6 = M2697()
    Case 2698: MC6 = M2698()
    Case 2699: MC6 = M2699()
    End Select
    Exit Function
EH:
    MC6 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC7(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2700: MC7 = M2700()
    Case 2701: MC7 = M2701()
    Case 2702: MC7 = M2702()
    Case 2703: MC7 = M2703()
    Case 2704: MC7 = M2704()
    Case 2705: MC7 = M2705()
    Case 2706: MC7 = M2706()
    Case 2707: MC7 = M2707()
    Case 2708: MC7 = M2708()
    Case 2709: MC7 = M2709()
    Case 2710: MC7 = M2710()
    Case 2711: MC7 = M2711()
    Case 2712: MC7 = M2712()
    Case 2713: MC7 = M2713()
    Case 2714: MC7 = M2714()
    Case 2715: MC7 = M2715()
    Case 2716: MC7 = M2716()
    Case 2717: MC7 = M2717()
    Case 2718: MC7 = M2718()
    Case 2719: MC7 = M2719()
    Case 2720: MC7 = M2720()
    Case 2721: MC7 = M2721()
    Case 2722: MC7 = M2722()
    Case 2723: MC7 = M2723()
    Case 2724: MC7 = M2724()
    Case 2725: MC7 = M2725()
    Case 2726: MC7 = M2726()
    Case 2727: MC7 = M2727()
    Case 2728: MC7 = M2728()
    Case 2729: MC7 = M2729()
    Case 2730: MC7 = M2730()
    Case 2731: MC7 = M2731()
    Case 2732: MC7 = M2732()
    Case 2733: MC7 = M2733()
    Case 2734: MC7 = M2734()
    Case 2735: MC7 = M2735()
    Case 2736: MC7 = M2736()
    Case 2737: MC7 = M2737()
    Case 2738: MC7 = M2738()
    Case 2739: MC7 = M2739()
    Case 2740: MC7 = M2740()
    Case 2741: MC7 = M2741()
    Case 2742: MC7 = M2742()
    Case 2743: MC7 = M2743()
    Case 2744: MC7 = M2744()
    Case 2745: MC7 = M2745()
    Case 2746: MC7 = M2746()
    Case 2747: MC7 = M2747()
    Case 2748: MC7 = M2748()
    Case 2749: MC7 = M2749()
    Case 2750: MC7 = M2750()
    Case 2751: MC7 = M2751()
    Case 2752: MC7 = M2752()
    Case 2753: MC7 = M2753()
    Case 2754: MC7 = M2754()
    Case 2755: MC7 = M2755()
    Case 2756: MC7 = M2756()
    Case 2757: MC7 = M2757()
    Case 2758: MC7 = M2758()
    Case 2759: MC7 = M2759()
    Case 2760: MC7 = M2760()
    Case 2761: MC7 = M2761()
    Case 2762: MC7 = M2762()
    Case 2763: MC7 = M2763()
    Case 2764: MC7 = M2764()
    Case 2765: MC7 = M2765()
    Case 2766: MC7 = M2766()
    Case 2767: MC7 = M2767()
    Case 2768: MC7 = M2768()
    Case 2769: MC7 = M2769()
    Case 2770: MC7 = M2770()
    Case 2771: MC7 = M2771()
    Case 2772: MC7 = M2772()
    Case 2773: MC7 = M2773()
    Case 2774: MC7 = M2774()
    Case 2775: MC7 = M2775()
    Case 2776: MC7 = M2776()
    Case 2777: MC7 = M2777()
    Case 2778: MC7 = M2778()
    Case 2779: MC7 = M2779()
    Case 2780: MC7 = M2780()
    Case 2781: MC7 = M2781()
    Case 2782: MC7 = M2782()
    Case 2783: MC7 = M2783()
    Case 2784: MC7 = M2784()
    Case 2785: MC7 = M2785()
    Case 2786: MC7 = M2786()
    Case 2787: MC7 = M2787()
    Case 2788: MC7 = M2788()
    Case 2789: MC7 = M2789()
    Case 2790: MC7 = M2790()
    Case 2791: MC7 = M2791()
    Case 2792: MC7 = M2792()
    Case 2793: MC7 = M2793()
    Case 2794: MC7 = M2794()
    Case 2795: MC7 = M2795()
    Case 2796: MC7 = M2796()
    Case 2797: MC7 = M2797()
    Case 2798: MC7 = M2798()
    Case 2799: MC7 = M2799()
    End Select
    Exit Function
EH:
    MC7 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC8(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2800: MC8 = M2800()
    Case 2801: MC8 = M2801()
    Case 2802: MC8 = M2802()
    Case 2803: MC8 = M2803()
    Case 2804: MC8 = M2804()
    Case 2805: MC8 = M2805()
    Case 2806: MC8 = M2806()
    Case 2807: MC8 = M2807()
    Case 2808: MC8 = M2808()
    Case 2809: MC8 = M2809()
    Case 2810: MC8 = M2810()
    Case 2811: MC8 = M2811()
    Case 2812: MC8 = M2812()
    Case 2813: MC8 = M2813()
    Case 2814: MC8 = M2814()
    Case 2815: MC8 = M2815()
    Case 2816: MC8 = M2816()
    Case 2817: MC8 = M2817()
    Case 2818: MC8 = M2818()
    Case 2819: MC8 = M2819()
    Case 2820: MC8 = M2820()
    Case 2821: MC8 = M2821()
    Case 2822: MC8 = M2822()
    Case 2823: MC8 = M2823()
    Case 2824: MC8 = M2824()
    Case 2825: MC8 = M2825()
    Case 2826: MC8 = M2826()
    Case 2827: MC8 = M2827()
    Case 2828: MC8 = M2828()
    Case 2829: MC8 = M2829()
    Case 2830: MC8 = M2830()
    Case 2831: MC8 = M2831()
    Case 2832: MC8 = M2832()
    Case 2833: MC8 = M2833()
    Case 2834: MC8 = M2834()
    Case 2835: MC8 = M2835()
    Case 2836: MC8 = M2836()
    Case 2837: MC8 = M2837()
    Case 2838: MC8 = M2838()
    Case 2839: MC8 = M2839()
    Case 2840: MC8 = M2840()
    Case 2841: MC8 = M2841()
    Case 2842: MC8 = M2842()
    Case 2843: MC8 = M2843()
    Case 2844: MC8 = M2844()
    Case 2845: MC8 = M2845()
    Case 2846: MC8 = M2846()
    Case 2847: MC8 = M2847()
    Case 2848: MC8 = M2848()
    Case 2849: MC8 = M2849()
    Case 2850: MC8 = M2850()
    Case 2851: MC8 = M2851()
    Case 2852: MC8 = M2852()
    Case 2853: MC8 = M2853()
    Case 2854: MC8 = M2854()
    Case 2855: MC8 = M2855()
    Case 2856: MC8 = M2856()
    Case 2857: MC8 = M2857()
    Case 2858: MC8 = M2858()
    Case 2859: MC8 = M2859()
    Case 2860: MC8 = M2860()
    Case 2861: MC8 = M2861()
    Case 2862: MC8 = M2862()
    Case 2863: MC8 = M2863()
    Case 2864: MC8 = M2864()
    Case 2865: MC8 = M2865()
    Case 2866: MC8 = M2866()
    Case 2867: MC8 = M2867()
    Case 2868: MC8 = M2868()
    Case 2869: MC8 = M2869()
    Case 2870: MC8 = M2870()
    Case 2871: MC8 = M2871()
    Case 2872: MC8 = M2872()
    Case 2873: MC8 = M2873()
    Case 2874: MC8 = M2874()
    Case 2875: MC8 = M2875()
    Case 2876: MC8 = M2876()
    Case 2877: MC8 = M2877()
    Case 2878: MC8 = M2878()
    Case 2879: MC8 = M2879()
    Case 2880: MC8 = M2880()
    Case 2881: MC8 = M2881()
    Case 2882: MC8 = M2882()
    Case 2883: MC8 = M2883()
    Case 2884: MC8 = M2884()
    Case 2885: MC8 = M2885()
    Case 2886: MC8 = M2886()
    Case 2887: MC8 = M2887()
    Case 2888: MC8 = M2888()
    Case 2889: MC8 = M2889()
    Case 2890: MC8 = M2890()
    Case 2891: MC8 = M2891()
    Case 2892: MC8 = M2892()
    Case 2893: MC8 = M2893()
    Case 2894: MC8 = M2894()
    Case 2895: MC8 = M2895()
    Case 2896: MC8 = M2896()
    Case 2897: MC8 = M2897()
    Case 2898: MC8 = M2898()
    Case 2899: MC8 = M2899()
    End Select
    Exit Function
EH:
    MC8 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function MC9(ByVal k As Long) As Variant
    On Error GoTo EH
    Select Case k
    Case 2900: MC9 = M2900()
    Case 2901: MC9 = M2901()
    Case 2902: MC9 = M2902()
    Case 2903: MC9 = M2903()
    Case 2904: MC9 = M2904()
    Case 2905: MC9 = M2905()
    Case 2906: MC9 = M2906()
    Case 2907: MC9 = M2907()
    Case 2908: MC9 = M2908()
    Case 2909: MC9 = M2909()
    Case 2910: MC9 = M2910()
    Case 2911: MC9 = M2911()
    Case 2912: MC9 = M2912()
    Case 2913: MC9 = M2913()
    Case 2914: MC9 = M2914()
    Case 2915: MC9 = M2915()
    Case 2916: MC9 = M2916()
    Case 2917: MC9 = M2917()
    Case 2918: MC9 = M2918()
    Case 2919: MC9 = M2919()
    Case 2920: MC9 = M2920()
    Case 2921: MC9 = M2921()
    Case 2922: MC9 = M2922()
    Case 2923: MC9 = M2923()
    Case 2924: MC9 = M2924()
    Case 2925: MC9 = M2925()
    Case 2926: MC9 = M2926()
    Case 2927: MC9 = M2927()
    Case 2928: MC9 = M2928()
    Case 2929: MC9 = M2929()
    Case 2930: MC9 = M2930()
    Case 2931: MC9 = M2931()
    Case 2932: MC9 = M2932()
    Case 2933: MC9 = M2933()
    Case 2934: MC9 = M2934()
    Case 2935: MC9 = M2935()
    Case 2936: MC9 = M2936()
    Case 2937: MC9 = M2937()
    Case 2938: MC9 = M2938()
    Case 2939: MC9 = M2939()
    Case 2940: MC9 = M2940()
    Case 2941: MC9 = M2941()
    Case 2942: MC9 = M2942()
    Case 2943: MC9 = M2943()
    Case 2944: MC9 = M2944()
    Case 2945: MC9 = M2945()
    Case 2946: MC9 = M2946()
    Case 2947: MC9 = M2947()
    Case 2948: MC9 = M2948()
    Case 2949: MC9 = M2949()
    Case 2950: MC9 = M2950()
    Case 2951: MC9 = M2951()
    Case 2952: MC9 = M2952()
    Case 2953: MC9 = M2953()
    Case 2954: MC9 = M2954()
    Case 2955: MC9 = M2955()
    Case 2956: MC9 = M2956()
    Case 2957: MC9 = M2957()
    Case 2958: MC9 = M2958()
    Case 2959: MC9 = M2959()
    Case 2960: MC9 = M2960()
    Case 2961: MC9 = M2961()
    Case 2962: MC9 = M2962()
    Case 2963: MC9 = M2963()
    Case 2964: MC9 = M2964()
    Case 2965: MC9 = M2965()
    Case 2966: MC9 = M2966()
    Case 2967: MC9 = M2967()
    Case 2968: MC9 = M2968()
    Case 2969: MC9 = M2969()
    Case 2970: MC9 = M2970()
    Case 2971: MC9 = M2971()
    Case 2972: MC9 = M2972()
    Case 2973: MC9 = M2973()
    Case 2974: MC9 = M2974()
    Case 2975: MC9 = M2975()
    Case 2976: MC9 = M2976()
    Case 2977: MC9 = M2977()
    Case 2978: MC9 = M2978()
    Case 2979: MC9 = M2979()
    Case 2980: MC9 = M2980()
    Case 2981: MC9 = M2981()
    Case 2982: MC9 = M2982()
    Case 2983: MC9 = M2983()
    Case 2984: MC9 = M2984()
    Case 2985: MC9 = M2985()
    Case 2986: MC9 = M2986()
    Case 2987: MC9 = M2987()
    Case 2988: MC9 = M2988()
    Case 2989: MC9 = M2989()
    Case 2990: MC9 = M2990()
    Case 2991: MC9 = M2991()
    Case 2992: MC9 = M2992()
    Case 2993: MC9 = M2993()
    Case 2994: MC9 = M2994()
    Case 2995: MC9 = M2995()
    Case 2996: MC9 = M2996()
    Case 2997: MC9 = M2997()
    Case 2998: MC9 = M2998()
    Case 2999: MC9 = M2999()
    End Select
    Exit Function
EH:
    MC9 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2000() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Trim(s)))
    M2000 = R
    Exit Function
EH:
    M2000 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2001() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Trim(s)))
    M2001 = R
    Exit Function
EH:
    M2001 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2002() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(LTrim(s)))
    M2002 = R
    Exit Function
EH:
    M2002 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2003() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(LTrim(s)))
    M2003 = R
    Exit Function
EH:
    M2003 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2004() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(LTrim(s)))
    M2004 = R
    Exit Function
EH:
    M2004 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2005() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(LTrim(s)))
    M2005 = R
    Exit Function
EH:
    M2005 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2006() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(LTrim(s)))
    M2006 = R
    Exit Function
EH:
    M2006 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2007() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(LTrim(s)))
    M2007 = R
    Exit Function
EH:
    M2007 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2008() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(LTrim(s)))
    M2008 = R
    Exit Function
EH:
    M2008 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2009() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(LTrim(s)))
    M2009 = R
    Exit Function
EH:
    M2009 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2010() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(LTrim(s)))
    M2010 = R
    Exit Function
EH:
    M2010 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2011() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(LTrim(s)))
    M2011 = R
    Exit Function
EH:
    M2011 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2012() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(LTrim(s)))
    M2012 = R
    Exit Function
EH:
    M2012 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2013() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(LTrim(s)))
    M2013 = R
    Exit Function
EH:
    M2013 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2014() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(LTrim(s)))
    M2014 = R
    Exit Function
EH:
    M2014 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2015() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(LTrim(s)))
    M2015 = R
    Exit Function
EH:
    M2015 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2016() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(LTrim(s)))
    M2016 = R
    Exit Function
EH:
    M2016 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2017() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(LTrim(s)))
    M2017 = R
    Exit Function
EH:
    M2017 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2018() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(LTrim(s)))
    M2018 = R
    Exit Function
EH:
    M2018 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2019() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(LTrim(s)))
    M2019 = R
    Exit Function
EH:
    M2019 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2020() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(LTrim(s)))
    M2020 = R
    Exit Function
EH:
    M2020 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2021() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(LTrim(s)))
    M2021 = R
    Exit Function
EH:
    M2021 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2022() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(RTrim(s)))
    M2022 = R
    Exit Function
EH:
    M2022 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2023() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(RTrim(s)))
    M2023 = R
    Exit Function
EH:
    M2023 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2024() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(RTrim(s)))
    M2024 = R
    Exit Function
EH:
    M2024 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2025() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(RTrim(s)))
    M2025 = R
    Exit Function
EH:
    M2025 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2026() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(RTrim(s)))
    M2026 = R
    Exit Function
EH:
    M2026 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2027() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(RTrim(s)))
    M2027 = R
    Exit Function
EH:
    M2027 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2028() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(RTrim(s)))
    M2028 = R
    Exit Function
EH:
    M2028 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2029() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(RTrim(s)))
    M2029 = R
    Exit Function
EH:
    M2029 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2030() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(RTrim(s)))
    M2030 = R
    Exit Function
EH:
    M2030 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2031() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(RTrim(s)))
    M2031 = R
    Exit Function
EH:
    M2031 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2032() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(RTrim(s)))
    M2032 = R
    Exit Function
EH:
    M2032 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2033() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(RTrim(s)))
    M2033 = R
    Exit Function
EH:
    M2033 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2034() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(RTrim(s)))
    M2034 = R
    Exit Function
EH:
    M2034 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2035() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(RTrim(s)))
    M2035 = R
    Exit Function
EH:
    M2035 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2036() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(RTrim(s)))
    M2036 = R
    Exit Function
EH:
    M2036 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2037() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(RTrim(s)))
    M2037 = R
    Exit Function
EH:
    M2037 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2038() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(RTrim(s)))
    M2038 = R
    Exit Function
EH:
    M2038 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2039() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(RTrim(s)))
    M2039 = R
    Exit Function
EH:
    M2039 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2040() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(RTrim(s)))
    M2040 = R
    Exit Function
EH:
    M2040 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2041() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(RTrim(s)))
    M2041 = R
    Exit Function
EH:
    M2041 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2042() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Len(s)))
    M2042 = R
    Exit Function
EH:
    M2042 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2043() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Len(s)))
    M2043 = R
    Exit Function
EH:
    M2043 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2044() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Len(s)))
    M2044 = R
    Exit Function
EH:
    M2044 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2045() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Len(s)))
    M2045 = R
    Exit Function
EH:
    M2045 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2046() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Len(s)))
    M2046 = R
    Exit Function
EH:
    M2046 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2047() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Len(s)))
    M2047 = R
    Exit Function
EH:
    M2047 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2048() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Len(s)))
    M2048 = R
    Exit Function
EH:
    M2048 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2049() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Len(s)))
    M2049 = R
    Exit Function
EH:
    M2049 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2050() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Len(s)))
    M2050 = R
    Exit Function
EH:
    M2050 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2051() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Len(s)))
    M2051 = R
    Exit Function
EH:
    M2051 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2052() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Len(s)))
    M2052 = R
    Exit Function
EH:
    M2052 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2053() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Len(s)))
    M2053 = R
    Exit Function
EH:
    M2053 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2054() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Len(s)))
    M2054 = R
    Exit Function
EH:
    M2054 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2055() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Len(s)))
    M2055 = R
    Exit Function
EH:
    M2055 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2056() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Len(s)))
    M2056 = R
    Exit Function
EH:
    M2056 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2057() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Len(s)))
    M2057 = R
    Exit Function
EH:
    M2057 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2058() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Len(s)))
    M2058 = R
    Exit Function
EH:
    M2058 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2059() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Len(s)))
    M2059 = R
    Exit Function
EH:
    M2059 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2060() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Len(s)))
    M2060 = R
    Exit Function
EH:
    M2060 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2061() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Len(s)))
    M2061 = R
    Exit Function
EH:
    M2061 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2062() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrReverse(s)))
    M2062 = R
    Exit Function
EH:
    M2062 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2063() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrReverse(s)))
    M2063 = R
    Exit Function
EH:
    M2063 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2064() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrReverse(s)))
    M2064 = R
    Exit Function
EH:
    M2064 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2065() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrReverse(s)))
    M2065 = R
    Exit Function
EH:
    M2065 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2066() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrReverse(s)))
    M2066 = R
    Exit Function
EH:
    M2066 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2067() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrReverse(s)))
    M2067 = R
    Exit Function
EH:
    M2067 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2068() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrReverse(s)))
    M2068 = R
    Exit Function
EH:
    M2068 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2069() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrReverse(s)))
    M2069 = R
    Exit Function
EH:
    M2069 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2070() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrReverse(s)))
    M2070 = R
    Exit Function
EH:
    M2070 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2071() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrReverse(s)))
    M2071 = R
    Exit Function
EH:
    M2071 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2072() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrReverse(s)))
    M2072 = R
    Exit Function
EH:
    M2072 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2073() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrReverse(s)))
    M2073 = R
    Exit Function
EH:
    M2073 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2074() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrReverse(s)))
    M2074 = R
    Exit Function
EH:
    M2074 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2075() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrReverse(s)))
    M2075 = R
    Exit Function
EH:
    M2075 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2076() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrReverse(s)))
    M2076 = R
    Exit Function
EH:
    M2076 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2077() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrReverse(s)))
    M2077 = R
    Exit Function
EH:
    M2077 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2078() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrReverse(s)))
    M2078 = R
    Exit Function
EH:
    M2078 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2079() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrReverse(s)))
    M2079 = R
    Exit Function
EH:
    M2079 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2080() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrReverse(s)))
    M2080 = R
    Exit Function
EH:
    M2080 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2081() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrReverse(s)))
    M2081 = R
    Exit Function
EH:
    M2081 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2082() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Asc(s)))
    M2082 = R
    Exit Function
EH:
    M2082 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2083() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Asc(s)))
    M2083 = R
    Exit Function
EH:
    M2083 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2084() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Asc(s)))
    M2084 = R
    Exit Function
EH:
    M2084 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2085() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Asc(s)))
    M2085 = R
    Exit Function
EH:
    M2085 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2086() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Asc(s)))
    M2086 = R
    Exit Function
EH:
    M2086 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2087() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Asc(s)))
    M2087 = R
    Exit Function
EH:
    M2087 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2088() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Asc(s)))
    M2088 = R
    Exit Function
EH:
    M2088 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2089() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Asc(s)))
    M2089 = R
    Exit Function
EH:
    M2089 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2090() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Asc(s)))
    M2090 = R
    Exit Function
EH:
    M2090 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2091() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Asc(s)))
    M2091 = R
    Exit Function
EH:
    M2091 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2092() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Asc(s)))
    M2092 = R
    Exit Function
EH:
    M2092 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2093() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Asc(s)))
    M2093 = R
    Exit Function
EH:
    M2093 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2094() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Asc(s)))
    M2094 = R
    Exit Function
EH:
    M2094 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2095() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Asc(s)))
    M2095 = R
    Exit Function
EH:
    M2095 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2096() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Asc(s)))
    M2096 = R
    Exit Function
EH:
    M2096 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2097() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Asc(s)))
    M2097 = R
    Exit Function
EH:
    M2097 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2098() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Asc(s)))
    M2098 = R
    Exit Function
EH:
    M2098 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2099() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Asc(s)))
    M2099 = R
    Exit Function
EH:
    M2099 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2100() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Asc(s)))
    M2100 = R
    Exit Function
EH:
    M2100 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2101() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Asc(s)))
    M2101 = R
    Exit Function
EH:
    M2101 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2102() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(AscW(s)))
    M2102 = R
    Exit Function
EH:
    M2102 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2103() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(AscW(s)))
    M2103 = R
    Exit Function
EH:
    M2103 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2104() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(AscW(s)))
    M2104 = R
    Exit Function
EH:
    M2104 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2105() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(AscW(s)))
    M2105 = R
    Exit Function
EH:
    M2105 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2106() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(AscW(s)))
    M2106 = R
    Exit Function
EH:
    M2106 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2107() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(AscW(s)))
    M2107 = R
    Exit Function
EH:
    M2107 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2108() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(AscW(s)))
    M2108 = R
    Exit Function
EH:
    M2108 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2109() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(AscW(s)))
    M2109 = R
    Exit Function
EH:
    M2109 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2110() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(AscW(s)))
    M2110 = R
    Exit Function
EH:
    M2110 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2111() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(AscW(s)))
    M2111 = R
    Exit Function
EH:
    M2111 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2112() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(AscW(s)))
    M2112 = R
    Exit Function
EH:
    M2112 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2113() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(AscW(s)))
    M2113 = R
    Exit Function
EH:
    M2113 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2114() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(AscW(s)))
    M2114 = R
    Exit Function
EH:
    M2114 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2115() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(AscW(s)))
    M2115 = R
    Exit Function
EH:
    M2115 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2116() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(AscW(s)))
    M2116 = R
    Exit Function
EH:
    M2116 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2117() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(AscW(s)))
    M2117 = R
    Exit Function
EH:
    M2117 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2118() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(AscW(s)))
    M2118 = R
    Exit Function
EH:
    M2118 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2119() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(AscW(s)))
    M2119 = R
    Exit Function
EH:
    M2119 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2120() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(AscW(s)))
    M2120 = R
    Exit Function
EH:
    M2120 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2121() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(AscW(s)))
    M2121 = R
    Exit Function
EH:
    M2121 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2122() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Left$(s, 2)))
    M2122 = R
    Exit Function
EH:
    M2122 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2123() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Left$(s, 2)))
    M2123 = R
    Exit Function
EH:
    M2123 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2124() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Left$(s, 2)))
    M2124 = R
    Exit Function
EH:
    M2124 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2125() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Left$(s, 2)))
    M2125 = R
    Exit Function
EH:
    M2125 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2126() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Left$(s, 2)))
    M2126 = R
    Exit Function
EH:
    M2126 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2127() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Left$(s, 2)))
    M2127 = R
    Exit Function
EH:
    M2127 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2128() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Left$(s, 2)))
    M2128 = R
    Exit Function
EH:
    M2128 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2129() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Left$(s, 2)))
    M2129 = R
    Exit Function
EH:
    M2129 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2130() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Left$(s, 2)))
    M2130 = R
    Exit Function
EH:
    M2130 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2131() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Left$(s, 2)))
    M2131 = R
    Exit Function
EH:
    M2131 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2132() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Left$(s, 2)))
    M2132 = R
    Exit Function
EH:
    M2132 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2133() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Left$(s, 2)))
    M2133 = R
    Exit Function
EH:
    M2133 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2134() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Left$(s, 2)))
    M2134 = R
    Exit Function
EH:
    M2134 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2135() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Left$(s, 2)))
    M2135 = R
    Exit Function
EH:
    M2135 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2136() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Left$(s, 2)))
    M2136 = R
    Exit Function
EH:
    M2136 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2137() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Left$(s, 2)))
    M2137 = R
    Exit Function
EH:
    M2137 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2138() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Left$(s, 2)))
    M2138 = R
    Exit Function
EH:
    M2138 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2139() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Left$(s, 2)))
    M2139 = R
    Exit Function
EH:
    M2139 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2140() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Left$(s, 2)))
    M2140 = R
    Exit Function
EH:
    M2140 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2141() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Left$(s, 2)))
    M2141 = R
    Exit Function
EH:
    M2141 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2142() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Right$(s, 2)))
    M2142 = R
    Exit Function
EH:
    M2142 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2143() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Right$(s, 2)))
    M2143 = R
    Exit Function
EH:
    M2143 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2144() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Right$(s, 2)))
    M2144 = R
    Exit Function
EH:
    M2144 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2145() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Right$(s, 2)))
    M2145 = R
    Exit Function
EH:
    M2145 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2146() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Right$(s, 2)))
    M2146 = R
    Exit Function
EH:
    M2146 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2147() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Right$(s, 2)))
    M2147 = R
    Exit Function
EH:
    M2147 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2148() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Right$(s, 2)))
    M2148 = R
    Exit Function
EH:
    M2148 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2149() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Right$(s, 2)))
    M2149 = R
    Exit Function
EH:
    M2149 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2150() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Right$(s, 2)))
    M2150 = R
    Exit Function
EH:
    M2150 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2151() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Right$(s, 2)))
    M2151 = R
    Exit Function
EH:
    M2151 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2152() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Right$(s, 2)))
    M2152 = R
    Exit Function
EH:
    M2152 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2153() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Right$(s, 2)))
    M2153 = R
    Exit Function
EH:
    M2153 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2154() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Right$(s, 2)))
    M2154 = R
    Exit Function
EH:
    M2154 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2155() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Right$(s, 2)))
    M2155 = R
    Exit Function
EH:
    M2155 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2156() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Right$(s, 2)))
    M2156 = R
    Exit Function
EH:
    M2156 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2157() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Right$(s, 2)))
    M2157 = R
    Exit Function
EH:
    M2157 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2158() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Right$(s, 2)))
    M2158 = R
    Exit Function
EH:
    M2158 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2159() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Right$(s, 2)))
    M2159 = R
    Exit Function
EH:
    M2159 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2160() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Right$(s, 2)))
    M2160 = R
    Exit Function
EH:
    M2160 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2161() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Right$(s, 2)))
    M2161 = R
    Exit Function
EH:
    M2161 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2162() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Mid$(s, 2)))
    M2162 = R
    Exit Function
EH:
    M2162 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2163() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Mid$(s, 2)))
    M2163 = R
    Exit Function
EH:
    M2163 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2164() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Mid$(s, 2)))
    M2164 = R
    Exit Function
EH:
    M2164 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2165() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Mid$(s, 2)))
    M2165 = R
    Exit Function
EH:
    M2165 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2166() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Mid$(s, 2)))
    M2166 = R
    Exit Function
EH:
    M2166 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2167() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Mid$(s, 2)))
    M2167 = R
    Exit Function
EH:
    M2167 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2168() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Mid$(s, 2)))
    M2168 = R
    Exit Function
EH:
    M2168 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2169() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Mid$(s, 2)))
    M2169 = R
    Exit Function
EH:
    M2169 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2170() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Mid$(s, 2)))
    M2170 = R
    Exit Function
EH:
    M2170 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2171() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Mid$(s, 2)))
    M2171 = R
    Exit Function
EH:
    M2171 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2172() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Mid$(s, 2)))
    M2172 = R
    Exit Function
EH:
    M2172 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2173() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Mid$(s, 2)))
    M2173 = R
    Exit Function
EH:
    M2173 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2174() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Mid$(s, 2)))
    M2174 = R
    Exit Function
EH:
    M2174 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2175() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Mid$(s, 2)))
    M2175 = R
    Exit Function
EH:
    M2175 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2176() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Mid$(s, 2)))
    M2176 = R
    Exit Function
EH:
    M2176 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2177() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Mid$(s, 2)))
    M2177 = R
    Exit Function
EH:
    M2177 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2178() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Mid$(s, 2)))
    M2178 = R
    Exit Function
EH:
    M2178 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2179() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Mid$(s, 2)))
    M2179 = R
    Exit Function
EH:
    M2179 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2180() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Mid$(s, 2)))
    M2180 = R
    Exit Function
EH:
    M2180 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2181() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Mid$(s, 2)))
    M2181 = R
    Exit Function
EH:
    M2181 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2182() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Mid$(s, 2, 1)))
    M2182 = R
    Exit Function
EH:
    M2182 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2183() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2183 = R
    Exit Function
EH:
    M2183 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2184() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2184 = R
    Exit Function
EH:
    M2184 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2185() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2185 = R
    Exit Function
EH:
    M2185 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2186() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Mid$(s, 2, 1)))
    M2186 = R
    Exit Function
EH:
    M2186 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2187() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2187 = R
    Exit Function
EH:
    M2187 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2188() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2188 = R
    Exit Function
EH:
    M2188 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2189() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2189 = R
    Exit Function
EH:
    M2189 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2190() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2190 = R
    Exit Function
EH:
    M2190 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2191() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2191 = R
    Exit Function
EH:
    M2191 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2192() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Mid$(s, 2, 1)))
    M2192 = R
    Exit Function
EH:
    M2192 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2193() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2193 = R
    Exit Function
EH:
    M2193 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2194() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2194 = R
    Exit Function
EH:
    M2194 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2195() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2195 = R
    Exit Function
EH:
    M2195 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2196() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Mid$(s, 2, 1)))
    M2196 = R
    Exit Function
EH:
    M2196 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2197() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Mid$(s, 2, 1)))
    M2197 = R
    Exit Function
EH:
    M2197 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2198() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2198 = R
    Exit Function
EH:
    M2198 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2199() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2199 = R
    Exit Function
EH:
    M2199 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2200() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2200 = R
    Exit Function
EH:
    M2200 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2201() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Mid$(s, 2, 1)))
    M2201 = R
    Exit Function
EH:
    M2201 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2202() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2202 = R
    Exit Function
EH:
    M2202 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2203() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2203 = R
    Exit Function
EH:
    M2203 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2204() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2204 = R
    Exit Function
EH:
    M2204 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2205() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2205 = R
    Exit Function
EH:
    M2205 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2206() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2206 = R
    Exit Function
EH:
    M2206 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2207() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2207 = R
    Exit Function
EH:
    M2207 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2208() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2208 = R
    Exit Function
EH:
    M2208 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2209() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2209 = R
    Exit Function
EH:
    M2209 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2210() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2210 = R
    Exit Function
EH:
    M2210 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2211() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2211 = R
    Exit Function
EH:
    M2211 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2212() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2212 = R
    Exit Function
EH:
    M2212 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2213() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2213 = R
    Exit Function
EH:
    M2213 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2214() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2214 = R
    Exit Function
EH:
    M2214 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2215() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2215 = R
    Exit Function
EH:
    M2215 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2216() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2216 = R
    Exit Function
EH:
    M2216 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2217() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2217 = R
    Exit Function
EH:
    M2217 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2218() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2218 = R
    Exit Function
EH:
    M2218 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2219() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2219 = R
    Exit Function
EH:
    M2219 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2220() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2220 = R
    Exit Function
EH:
    M2220 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2221() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbUpperCase)))
    M2221 = R
    Exit Function
EH:
    M2221 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2222() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2222 = R
    Exit Function
EH:
    M2222 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2223() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2223 = R
    Exit Function
EH:
    M2223 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2224() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2224 = R
    Exit Function
EH:
    M2224 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2225() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2225 = R
    Exit Function
EH:
    M2225 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2226() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2226 = R
    Exit Function
EH:
    M2226 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2227() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2227 = R
    Exit Function
EH:
    M2227 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2228() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2228 = R
    Exit Function
EH:
    M2228 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2229() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2229 = R
    Exit Function
EH:
    M2229 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2230() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2230 = R
    Exit Function
EH:
    M2230 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2231() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2231 = R
    Exit Function
EH:
    M2231 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2232() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2232 = R
    Exit Function
EH:
    M2232 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2233() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2233 = R
    Exit Function
EH:
    M2233 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2234() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2234 = R
    Exit Function
EH:
    M2234 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2235() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2235 = R
    Exit Function
EH:
    M2235 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2236() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2236 = R
    Exit Function
EH:
    M2236 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2237() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2237 = R
    Exit Function
EH:
    M2237 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2238() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2238 = R
    Exit Function
EH:
    M2238 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2239() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2239 = R
    Exit Function
EH:
    M2239 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2240() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2240 = R
    Exit Function
EH:
    M2240 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2241() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbLowerCase)))
    M2241 = R
    Exit Function
EH:
    M2241 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2242() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2242 = R
    Exit Function
EH:
    M2242 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2243() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2243 = R
    Exit Function
EH:
    M2243 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2244() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2244 = R
    Exit Function
EH:
    M2244 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2245() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2245 = R
    Exit Function
EH:
    M2245 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2246() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2246 = R
    Exit Function
EH:
    M2246 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2247() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2247 = R
    Exit Function
EH:
    M2247 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2248() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2248 = R
    Exit Function
EH:
    M2248 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2249() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2249 = R
    Exit Function
EH:
    M2249 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2250() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2250 = R
    Exit Function
EH:
    M2250 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2251() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2251 = R
    Exit Function
EH:
    M2251 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2252() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2252 = R
    Exit Function
EH:
    M2252 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2253() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2253 = R
    Exit Function
EH:
    M2253 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2254() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2254 = R
    Exit Function
EH:
    M2254 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2255() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2255 = R
    Exit Function
EH:
    M2255 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2256() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2256 = R
    Exit Function
EH:
    M2256 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2257() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2257 = R
    Exit Function
EH:
    M2257 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2258() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2258 = R
    Exit Function
EH:
    M2258 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2259() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2259 = R
    Exit Function
EH:
    M2259 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2260() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2260 = R
    Exit Function
EH:
    M2260 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2261() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbProperCase)))
    M2261 = R
    Exit Function
EH:
    M2261 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2262() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbWide)))
    M2262 = R
    Exit Function
EH:
    M2262 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2263() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbWide)))
    M2263 = R
    Exit Function
EH:
    M2263 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2264() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbWide)))
    M2264 = R
    Exit Function
EH:
    M2264 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2265() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbWide)))
    M2265 = R
    Exit Function
EH:
    M2265 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2266() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbWide)))
    M2266 = R
    Exit Function
EH:
    M2266 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2267() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbWide)))
    M2267 = R
    Exit Function
EH:
    M2267 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2268() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbWide)))
    M2268 = R
    Exit Function
EH:
    M2268 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2269() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbWide)))
    M2269 = R
    Exit Function
EH:
    M2269 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2270() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbWide)))
    M2270 = R
    Exit Function
EH:
    M2270 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2271() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbWide)))
    M2271 = R
    Exit Function
EH:
    M2271 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2272() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbWide)))
    M2272 = R
    Exit Function
EH:
    M2272 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2273() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbWide)))
    M2273 = R
    Exit Function
EH:
    M2273 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2274() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbWide)))
    M2274 = R
    Exit Function
EH:
    M2274 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2275() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbWide)))
    M2275 = R
    Exit Function
EH:
    M2275 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2276() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbWide)))
    M2276 = R
    Exit Function
EH:
    M2276 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2277() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbWide)))
    M2277 = R
    Exit Function
EH:
    M2277 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2278() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbWide)))
    M2278 = R
    Exit Function
EH:
    M2278 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2279() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbWide)))
    M2279 = R
    Exit Function
EH:
    M2279 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2280() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbWide)))
    M2280 = R
    Exit Function
EH:
    M2280 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2281() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbWide)))
    M2281 = R
    Exit Function
EH:
    M2281 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2282() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2282 = R
    Exit Function
EH:
    M2282 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2283() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2283 = R
    Exit Function
EH:
    M2283 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2284() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2284 = R
    Exit Function
EH:
    M2284 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2285() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2285 = R
    Exit Function
EH:
    M2285 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2286() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2286 = R
    Exit Function
EH:
    M2286 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2287() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2287 = R
    Exit Function
EH:
    M2287 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2288() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2288 = R
    Exit Function
EH:
    M2288 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2289() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2289 = R
    Exit Function
EH:
    M2289 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2290() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2290 = R
    Exit Function
EH:
    M2290 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2291() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2291 = R
    Exit Function
EH:
    M2291 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2292() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2292 = R
    Exit Function
EH:
    M2292 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2293() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2293 = R
    Exit Function
EH:
    M2293 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2294() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2294 = R
    Exit Function
EH:
    M2294 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2295() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2295 = R
    Exit Function
EH:
    M2295 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2296() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2296 = R
    Exit Function
EH:
    M2296 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2297() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2297 = R
    Exit Function
EH:
    M2297 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2298() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2298 = R
    Exit Function
EH:
    M2298 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2299() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2299 = R
    Exit Function
EH:
    M2299 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2300() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2300 = R
    Exit Function
EH:
    M2300 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2301() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbNarrow)))
    M2301 = R
    Exit Function
EH:
    M2301 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2302() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2302 = R
    Exit Function
EH:
    M2302 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2303() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2303 = R
    Exit Function
EH:
    M2303 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2304() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2304 = R
    Exit Function
EH:
    M2304 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2305() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2305 = R
    Exit Function
EH:
    M2305 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2306() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2306 = R
    Exit Function
EH:
    M2306 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2307() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2307 = R
    Exit Function
EH:
    M2307 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2308() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2308 = R
    Exit Function
EH:
    M2308 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2309() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2309 = R
    Exit Function
EH:
    M2309 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2310() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2310 = R
    Exit Function
EH:
    M2310 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2311() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2311 = R
    Exit Function
EH:
    M2311 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2312() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2312 = R
    Exit Function
EH:
    M2312 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2313() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2313 = R
    Exit Function
EH:
    M2313 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2314() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2314 = R
    Exit Function
EH:
    M2314 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2315() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2315 = R
    Exit Function
EH:
    M2315 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2316() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2316 = R
    Exit Function
EH:
    M2316 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2317() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2317 = R
    Exit Function
EH:
    M2317 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2318() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2318 = R
    Exit Function
EH:
    M2318 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2319() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2319 = R
    Exit Function
EH:
    M2319 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2320() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2320 = R
    Exit Function
EH:
    M2320 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2321() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbUpperCase + vbWide)))
    M2321 = R
    Exit Function
EH:
    M2321 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2322() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2322 = R
    Exit Function
EH:
    M2322 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2323() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2323 = R
    Exit Function
EH:
    M2323 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2324() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2324 = R
    Exit Function
EH:
    M2324 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2325() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2325 = R
    Exit Function
EH:
    M2325 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2326() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2326 = R
    Exit Function
EH:
    M2326 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2327() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2327 = R
    Exit Function
EH:
    M2327 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2328() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2328 = R
    Exit Function
EH:
    M2328 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2329() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2329 = R
    Exit Function
EH:
    M2329 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2330() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2330 = R
    Exit Function
EH:
    M2330 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2331() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2331 = R
    Exit Function
EH:
    M2331 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2332() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2332 = R
    Exit Function
EH:
    M2332 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2333() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2333 = R
    Exit Function
EH:
    M2333 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2334() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2334 = R
    Exit Function
EH:
    M2334 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2335() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2335 = R
    Exit Function
EH:
    M2335 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2336() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2336 = R
    Exit Function
EH:
    M2336 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2337() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2337 = R
    Exit Function
EH:
    M2337 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2338() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2338 = R
    Exit Function
EH:
    M2338 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2339() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2339 = R
    Exit Function
EH:
    M2339 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2340() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2340 = R
    Exit Function
EH:
    M2340 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2341() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbProperCase, 1033)))
    M2341 = R
    Exit Function
EH:
    M2341 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2342() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2342 = R
    Exit Function
EH:
    M2342 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2343() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2343 = R
    Exit Function
EH:
    M2343 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2344() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2344 = R
    Exit Function
EH:
    M2344 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2345() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2345 = R
    Exit Function
EH:
    M2345 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2346() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2346 = R
    Exit Function
EH:
    M2346 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2347() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2347 = R
    Exit Function
EH:
    M2347 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2348() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2348 = R
    Exit Function
EH:
    M2348 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2349() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2349 = R
    Exit Function
EH:
    M2349 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2350() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2350 = R
    Exit Function
EH:
    M2350 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2351() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2351 = R
    Exit Function
EH:
    M2351 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2352() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2352 = R
    Exit Function
EH:
    M2352 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2353() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2353 = R
    Exit Function
EH:
    M2353 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2354() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2354 = R
    Exit Function
EH:
    M2354 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2355() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2355 = R
    Exit Function
EH:
    M2355 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2356() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2356 = R
    Exit Function
EH:
    M2356 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2357() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2357 = R
    Exit Function
EH:
    M2357 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2358() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2358 = R
    Exit Function
EH:
    M2358 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2359() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2359 = R
    Exit Function
EH:
    M2359 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2360() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2360 = R
    Exit Function
EH:
    M2360 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2361() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbHiragana)))
    M2361 = R
    Exit Function
EH:
    M2361 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2362() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2362 = R
    Exit Function
EH:
    M2362 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2363() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2363 = R
    Exit Function
EH:
    M2363 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2364() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2364 = R
    Exit Function
EH:
    M2364 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2365() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2365 = R
    Exit Function
EH:
    M2365 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2366() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2366 = R
    Exit Function
EH:
    M2366 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2367() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2367 = R
    Exit Function
EH:
    M2367 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2368() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2368 = R
    Exit Function
EH:
    M2368 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2369() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2369 = R
    Exit Function
EH:
    M2369 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2370() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2370 = R
    Exit Function
EH:
    M2370 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2371() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2371 = R
    Exit Function
EH:
    M2371 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2372() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2372 = R
    Exit Function
EH:
    M2372 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2373() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2373 = R
    Exit Function
EH:
    M2373 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2374() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2374 = R
    Exit Function
EH:
    M2374 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2375() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2375 = R
    Exit Function
EH:
    M2375 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2376() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2376 = R
    Exit Function
EH:
    M2376 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2377() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2377 = R
    Exit Function
EH:
    M2377 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2378() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2378 = R
    Exit Function
EH:
    M2378 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2379() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2379 = R
    Exit Function
EH:
    M2379 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2380() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2380 = R
    Exit Function
EH:
    M2380 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2381() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbKatakana)))
    M2381 = R
    Exit Function
EH:
    M2381 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2382() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2382 = R
    Exit Function
EH:
    M2382 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2383() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2383 = R
    Exit Function
EH:
    M2383 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2384() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2384 = R
    Exit Function
EH:
    M2384 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2385() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2385 = R
    Exit Function
EH:
    M2385 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2386() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2386 = R
    Exit Function
EH:
    M2386 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2387() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2387 = R
    Exit Function
EH:
    M2387 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2388() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2388 = R
    Exit Function
EH:
    M2388 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2389() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2389 = R
    Exit Function
EH:
    M2389 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2390() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2390 = R
    Exit Function
EH:
    M2390 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2391() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2391 = R
    Exit Function
EH:
    M2391 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2392() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2392 = R
    Exit Function
EH:
    M2392 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2393() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2393 = R
    Exit Function
EH:
    M2393 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2394() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2394 = R
    Exit Function
EH:
    M2394 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2395() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2395 = R
    Exit Function
EH:
    M2395 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2396() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2396 = R
    Exit Function
EH:
    M2396 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2397() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2397 = R
    Exit Function
EH:
    M2397 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2398() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2398 = R
    Exit Function
EH:
    M2398 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2399() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2399 = R
    Exit Function
EH:
    M2399 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2400() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2400 = R
    Exit Function
EH:
    M2400 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2401() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbUnicode)))
    M2401 = R
    Exit Function
EH:
    M2401 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2402() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2402 = R
    Exit Function
EH:
    M2402 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2403() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2403 = R
    Exit Function
EH:
    M2403 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2404() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2404 = R
    Exit Function
EH:
    M2404 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2405() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2405 = R
    Exit Function
EH:
    M2405 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2406() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2406 = R
    Exit Function
EH:
    M2406 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2407() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2407 = R
    Exit Function
EH:
    M2407 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2408() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2408 = R
    Exit Function
EH:
    M2408 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2409() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2409 = R
    Exit Function
EH:
    M2409 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2410() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2410 = R
    Exit Function
EH:
    M2410 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2411() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2411 = R
    Exit Function
EH:
    M2411 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2412() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2412 = R
    Exit Function
EH:
    M2412 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2413() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2413 = R
    Exit Function
EH:
    M2413 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2414() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2414 = R
    Exit Function
EH:
    M2414 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2415() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2415 = R
    Exit Function
EH:
    M2415 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2416() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2416 = R
    Exit Function
EH:
    M2416 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2417() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2417 = R
    Exit Function
EH:
    M2417 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2418() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2418 = R
    Exit Function
EH:
    M2418 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2419() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2419 = R
    Exit Function
EH:
    M2419 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2420() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2420 = R
    Exit Function
EH:
    M2420 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2421() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, vbFromUnicode)))
    M2421 = R
    Exit Function
EH:
    M2421 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2422() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 0)))
    M2422 = R
    Exit Function
EH:
    M2422 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2423() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 0)))
    M2423 = R
    Exit Function
EH:
    M2423 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2424() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 0)))
    M2424 = R
    Exit Function
EH:
    M2424 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2425() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 0)))
    M2425 = R
    Exit Function
EH:
    M2425 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2426() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 0)))
    M2426 = R
    Exit Function
EH:
    M2426 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2427() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 0)))
    M2427 = R
    Exit Function
EH:
    M2427 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2428() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 0)))
    M2428 = R
    Exit Function
EH:
    M2428 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2429() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 0)))
    M2429 = R
    Exit Function
EH:
    M2429 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2430() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 0)))
    M2430 = R
    Exit Function
EH:
    M2430 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2431() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 0)))
    M2431 = R
    Exit Function
EH:
    M2431 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2432() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 0)))
    M2432 = R
    Exit Function
EH:
    M2432 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2433() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 0)))
    M2433 = R
    Exit Function
EH:
    M2433 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2434() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 0)))
    M2434 = R
    Exit Function
EH:
    M2434 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2435() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 0)))
    M2435 = R
    Exit Function
EH:
    M2435 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2436() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 0)))
    M2436 = R
    Exit Function
EH:
    M2436 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2437() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 0)))
    M2437 = R
    Exit Function
EH:
    M2437 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2438() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 0)))
    M2438 = R
    Exit Function
EH:
    M2438 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2439() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 0)))
    M2439 = R
    Exit Function
EH:
    M2439 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2440() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 0)))
    M2440 = R
    Exit Function
EH:
    M2440 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2441() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 0)))
    M2441 = R
    Exit Function
EH:
    M2441 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2442() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 3)))
    M2442 = R
    Exit Function
EH:
    M2442 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2443() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 3)))
    M2443 = R
    Exit Function
EH:
    M2443 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2444() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 3)))
    M2444 = R
    Exit Function
EH:
    M2444 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2445() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 3)))
    M2445 = R
    Exit Function
EH:
    M2445 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2446() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 3)))
    M2446 = R
    Exit Function
EH:
    M2446 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2447() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 3)))
    M2447 = R
    Exit Function
EH:
    M2447 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2448() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 3)))
    M2448 = R
    Exit Function
EH:
    M2448 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2449() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 3)))
    M2449 = R
    Exit Function
EH:
    M2449 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2450() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 3)))
    M2450 = R
    Exit Function
EH:
    M2450 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2451() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 3)))
    M2451 = R
    Exit Function
EH:
    M2451 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2452() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 3)))
    M2452 = R
    Exit Function
EH:
    M2452 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2453() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 3)))
    M2453 = R
    Exit Function
EH:
    M2453 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2454() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 3)))
    M2454 = R
    Exit Function
EH:
    M2454 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2455() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 3)))
    M2455 = R
    Exit Function
EH:
    M2455 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2456() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 3)))
    M2456 = R
    Exit Function
EH:
    M2456 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2457() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 3)))
    M2457 = R
    Exit Function
EH:
    M2457 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2458() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 3)))
    M2458 = R
    Exit Function
EH:
    M2458 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2459() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 3)))
    M2459 = R
    Exit Function
EH:
    M2459 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2460() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 3)))
    M2460 = R
    Exit Function
EH:
    M2460 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2461() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 3)))
    M2461 = R
    Exit Function
EH:
    M2461 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2462() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 5)))
    M2462 = R
    Exit Function
EH:
    M2462 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2463() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 5)))
    M2463 = R
    Exit Function
EH:
    M2463 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2464() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 5)))
    M2464 = R
    Exit Function
EH:
    M2464 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2465() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 5)))
    M2465 = R
    Exit Function
EH:
    M2465 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2466() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 5)))
    M2466 = R
    Exit Function
EH:
    M2466 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2467() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 5)))
    M2467 = R
    Exit Function
EH:
    M2467 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2468() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 5)))
    M2468 = R
    Exit Function
EH:
    M2468 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2469() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 5)))
    M2469 = R
    Exit Function
EH:
    M2469 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2470() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 5)))
    M2470 = R
    Exit Function
EH:
    M2470 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2471() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 5)))
    M2471 = R
    Exit Function
EH:
    M2471 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2472() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 5)))
    M2472 = R
    Exit Function
EH:
    M2472 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2473() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 5)))
    M2473 = R
    Exit Function
EH:
    M2473 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2474() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 5)))
    M2474 = R
    Exit Function
EH:
    M2474 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2475() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 5)))
    M2475 = R
    Exit Function
EH:
    M2475 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2476() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 5)))
    M2476 = R
    Exit Function
EH:
    M2476 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2477() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 5)))
    M2477 = R
    Exit Function
EH:
    M2477 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2478() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 5)))
    M2478 = R
    Exit Function
EH:
    M2478 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2479() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 5)))
    M2479 = R
    Exit Function
EH:
    M2479 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2480() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 5)))
    M2480 = R
    Exit Function
EH:
    M2480 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2481() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 5)))
    M2481 = R
    Exit Function
EH:
    M2481 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2482() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 7)))
    M2482 = R
    Exit Function
EH:
    M2482 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2483() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 7)))
    M2483 = R
    Exit Function
EH:
    M2483 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2484() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 7)))
    M2484 = R
    Exit Function
EH:
    M2484 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2485() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 7)))
    M2485 = R
    Exit Function
EH:
    M2485 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2486() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 7)))
    M2486 = R
    Exit Function
EH:
    M2486 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2487() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 7)))
    M2487 = R
    Exit Function
EH:
    M2487 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2488() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 7)))
    M2488 = R
    Exit Function
EH:
    M2488 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2489() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 7)))
    M2489 = R
    Exit Function
EH:
    M2489 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2490() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 7)))
    M2490 = R
    Exit Function
EH:
    M2490 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2491() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 7)))
    M2491 = R
    Exit Function
EH:
    M2491 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2492() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 7)))
    M2492 = R
    Exit Function
EH:
    M2492 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2493() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 7)))
    M2493 = R
    Exit Function
EH:
    M2493 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2494() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 7)))
    M2494 = R
    Exit Function
EH:
    M2494 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2495() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 7)))
    M2495 = R
    Exit Function
EH:
    M2495 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2496() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 7)))
    M2496 = R
    Exit Function
EH:
    M2496 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2497() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 7)))
    M2497 = R
    Exit Function
EH:
    M2497 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2498() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 7)))
    M2498 = R
    Exit Function
EH:
    M2498 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2499() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 7)))
    M2499 = R
    Exit Function
EH:
    M2499 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2500() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 7)))
    M2500 = R
    Exit Function
EH:
    M2500 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2501() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 7)))
    M2501 = R
    Exit Function
EH:
    M2501 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2502() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 9)))
    M2502 = R
    Exit Function
EH:
    M2502 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2503() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 9)))
    M2503 = R
    Exit Function
EH:
    M2503 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2504() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 9)))
    M2504 = R
    Exit Function
EH:
    M2504 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2505() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 9)))
    M2505 = R
    Exit Function
EH:
    M2505 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2506() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 9)))
    M2506 = R
    Exit Function
EH:
    M2506 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2507() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 9)))
    M2507 = R
    Exit Function
EH:
    M2507 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2508() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 9)))
    M2508 = R
    Exit Function
EH:
    M2508 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2509() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 9)))
    M2509 = R
    Exit Function
EH:
    M2509 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2510() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 9)))
    M2510 = R
    Exit Function
EH:
    M2510 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2511() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 9)))
    M2511 = R
    Exit Function
EH:
    M2511 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2512() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 9)))
    M2512 = R
    Exit Function
EH:
    M2512 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2513() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 9)))
    M2513 = R
    Exit Function
EH:
    M2513 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2514() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 9)))
    M2514 = R
    Exit Function
EH:
    M2514 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2515() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 9)))
    M2515 = R
    Exit Function
EH:
    M2515 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2516() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 9)))
    M2516 = R
    Exit Function
EH:
    M2516 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2517() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 9)))
    M2517 = R
    Exit Function
EH:
    M2517 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2518() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 9)))
    M2518 = R
    Exit Function
EH:
    M2518 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2519() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 9)))
    M2519 = R
    Exit Function
EH:
    M2519 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2520() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 9)))
    M2520 = R
    Exit Function
EH:
    M2520 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2521() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 9)))
    M2521 = R
    Exit Function
EH:
    M2521 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2522() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 17)))
    M2522 = R
    Exit Function
EH:
    M2522 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2523() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 17)))
    M2523 = R
    Exit Function
EH:
    M2523 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2524() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 17)))
    M2524 = R
    Exit Function
EH:
    M2524 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2525() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 17)))
    M2525 = R
    Exit Function
EH:
    M2525 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2526() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 17)))
    M2526 = R
    Exit Function
EH:
    M2526 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2527() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 17)))
    M2527 = R
    Exit Function
EH:
    M2527 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2528() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 17)))
    M2528 = R
    Exit Function
EH:
    M2528 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2529() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 17)))
    M2529 = R
    Exit Function
EH:
    M2529 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2530() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 17)))
    M2530 = R
    Exit Function
EH:
    M2530 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2531() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 17)))
    M2531 = R
    Exit Function
EH:
    M2531 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2532() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 17)))
    M2532 = R
    Exit Function
EH:
    M2532 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2533() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 17)))
    M2533 = R
    Exit Function
EH:
    M2533 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2534() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 17)))
    M2534 = R
    Exit Function
EH:
    M2534 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2535() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 17)))
    M2535 = R
    Exit Function
EH:
    M2535 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2536() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 17)))
    M2536 = R
    Exit Function
EH:
    M2536 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2537() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 17)))
    M2537 = R
    Exit Function
EH:
    M2537 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2538() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 17)))
    M2538 = R
    Exit Function
EH:
    M2538 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2539() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 17)))
    M2539 = R
    Exit Function
EH:
    M2539 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2540() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 17)))
    M2540 = R
    Exit Function
EH:
    M2540 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2541() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 17)))
    M2541 = R
    Exit Function
EH:
    M2541 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2542() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 32)))
    M2542 = R
    Exit Function
EH:
    M2542 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2543() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 32)))
    M2543 = R
    Exit Function
EH:
    M2543 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2544() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 32)))
    M2544 = R
    Exit Function
EH:
    M2544 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2545() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 32)))
    M2545 = R
    Exit Function
EH:
    M2545 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2546() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 32)))
    M2546 = R
    Exit Function
EH:
    M2546 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2547() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 32)))
    M2547 = R
    Exit Function
EH:
    M2547 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2548() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 32)))
    M2548 = R
    Exit Function
EH:
    M2548 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2549() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 32)))
    M2549 = R
    Exit Function
EH:
    M2549 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2550() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 32)))
    M2550 = R
    Exit Function
EH:
    M2550 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2551() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 32)))
    M2551 = R
    Exit Function
EH:
    M2551 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2552() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 32)))
    M2552 = R
    Exit Function
EH:
    M2552 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2553() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 32)))
    M2553 = R
    Exit Function
EH:
    M2553 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2554() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 32)))
    M2554 = R
    Exit Function
EH:
    M2554 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2555() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 32)))
    M2555 = R
    Exit Function
EH:
    M2555 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2556() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 32)))
    M2556 = R
    Exit Function
EH:
    M2556 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2557() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 32)))
    M2557 = R
    Exit Function
EH:
    M2557 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2558() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 32)))
    M2558 = R
    Exit Function
EH:
    M2558 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2559() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 32)))
    M2559 = R
    Exit Function
EH:
    M2559 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2560() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 32)))
    M2560 = R
    Exit Function
EH:
    M2560 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2561() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 32)))
    M2561 = R
    Exit Function
EH:
    M2561 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2562() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2562 = R
    Exit Function
EH:
    M2562 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2563() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2563 = R
    Exit Function
EH:
    M2563 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2564() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2564 = R
    Exit Function
EH:
    M2564 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2565() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2565 = R
    Exit Function
EH:
    M2565 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2566() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2566 = R
    Exit Function
EH:
    M2566 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2567() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2567 = R
    Exit Function
EH:
    M2567 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2568() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2568 = R
    Exit Function
EH:
    M2568 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2569() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2569 = R
    Exit Function
EH:
    M2569 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2570() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2570 = R
    Exit Function
EH:
    M2570 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2571() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2571 = R
    Exit Function
EH:
    M2571 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2572() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2572 = R
    Exit Function
EH:
    M2572 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2573() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2573 = R
    Exit Function
EH:
    M2573 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2574() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2574 = R
    Exit Function
EH:
    M2574 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2575() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2575 = R
    Exit Function
EH:
    M2575 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2576() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2576 = R
    Exit Function
EH:
    M2576 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2577() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2577 = R
    Exit Function
EH:
    M2577 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2578() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2578 = R
    Exit Function
EH:
    M2578 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2579() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2579 = R
    Exit Function
EH:
    M2579 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2580() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2580 = R
    Exit Function
EH:
    M2580 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2581() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 1, 1031)))
    M2581 = R
    Exit Function
EH:
    M2581 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2582() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2582 = R
    Exit Function
EH:
    M2582 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2583() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2583 = R
    Exit Function
EH:
    M2583 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2584() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2584 = R
    Exit Function
EH:
    M2584 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2585() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2585 = R
    Exit Function
EH:
    M2585 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2586() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2586 = R
    Exit Function
EH:
    M2586 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2587() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2587 = R
    Exit Function
EH:
    M2587 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2588() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2588 = R
    Exit Function
EH:
    M2588 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2589() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2589 = R
    Exit Function
EH:
    M2589 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2590() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2590 = R
    Exit Function
EH:
    M2590 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2591() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2591 = R
    Exit Function
EH:
    M2591 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2592() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2592 = R
    Exit Function
EH:
    M2592 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2593() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2593 = R
    Exit Function
EH:
    M2593 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2594() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2594 = R
    Exit Function
EH:
    M2594 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2595() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2595 = R
    Exit Function
EH:
    M2595 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2596() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2596 = R
    Exit Function
EH:
    M2596 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2597() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2597 = R
    Exit Function
EH:
    M2597 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2598() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2598 = R
    Exit Function
EH:
    M2598 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2599() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2599 = R
    Exit Function
EH:
    M2599 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2600() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2600 = R
    Exit Function
EH:
    M2600 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2601() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(StrConv(s, 2, 1055)))
    M2601 = R
    Exit Function
EH:
    M2601 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2602() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(LenB(s)))
    M2602 = R
    Exit Function
EH:
    M2602 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2603() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(LenB(s)))
    M2603 = R
    Exit Function
EH:
    M2603 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2604() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(LenB(s)))
    M2604 = R
    Exit Function
EH:
    M2604 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2605() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(LenB(s)))
    M2605 = R
    Exit Function
EH:
    M2605 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2606() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(LenB(s)))
    M2606 = R
    Exit Function
EH:
    M2606 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2607() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(LenB(s)))
    M2607 = R
    Exit Function
EH:
    M2607 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2608() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(LenB(s)))
    M2608 = R
    Exit Function
EH:
    M2608 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2609() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(LenB(s)))
    M2609 = R
    Exit Function
EH:
    M2609 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2610() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(LenB(s)))
    M2610 = R
    Exit Function
EH:
    M2610 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2611() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(LenB(s)))
    M2611 = R
    Exit Function
EH:
    M2611 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2612() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(LenB(s)))
    M2612 = R
    Exit Function
EH:
    M2612 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2613() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(LenB(s)))
    M2613 = R
    Exit Function
EH:
    M2613 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2614() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(LenB(s)))
    M2614 = R
    Exit Function
EH:
    M2614 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2615() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(LenB(s)))
    M2615 = R
    Exit Function
EH:
    M2615 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2616() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(LenB(s)))
    M2616 = R
    Exit Function
EH:
    M2616 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2617() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(LenB(s)))
    M2617 = R
    Exit Function
EH:
    M2617 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2618() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(LenB(s)))
    M2618 = R
    Exit Function
EH:
    M2618 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2619() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(LenB(s)))
    M2619 = R
    Exit Function
EH:
    M2619 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2620() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(LenB(s)))
    M2620 = R
    Exit Function
EH:
    M2620 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2621() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(LenB(s)))
    M2621 = R
    Exit Function
EH:
    M2621 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2622() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Space(Len(s))))
    M2622 = R
    Exit Function
EH:
    M2622 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2623() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Space(Len(s))))
    M2623 = R
    Exit Function
EH:
    M2623 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2624() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Space(Len(s))))
    M2624 = R
    Exit Function
EH:
    M2624 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2625() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Space(Len(s))))
    M2625 = R
    Exit Function
EH:
    M2625 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2626() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Space(Len(s))))
    M2626 = R
    Exit Function
EH:
    M2626 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2627() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Space(Len(s))))
    M2627 = R
    Exit Function
EH:
    M2627 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2628() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Space(Len(s))))
    M2628 = R
    Exit Function
EH:
    M2628 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2629() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Space(Len(s))))
    M2629 = R
    Exit Function
EH:
    M2629 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2630() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Space(Len(s))))
    M2630 = R
    Exit Function
EH:
    M2630 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2631() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Space(Len(s))))
    M2631 = R
    Exit Function
EH:
    M2631 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2632() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Space(Len(s))))
    M2632 = R
    Exit Function
EH:
    M2632 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2633() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Space(Len(s))))
    M2633 = R
    Exit Function
EH:
    M2633 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2634() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Space(Len(s))))
    M2634 = R
    Exit Function
EH:
    M2634 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2635() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Space(Len(s))))
    M2635 = R
    Exit Function
EH:
    M2635 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2636() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Space(Len(s))))
    M2636 = R
    Exit Function
EH:
    M2636 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2637() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Space(Len(s))))
    M2637 = R
    Exit Function
EH:
    M2637 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2638() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Space(Len(s))))
    M2638 = R
    Exit Function
EH:
    M2638 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2639() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Space(Len(s))))
    M2639 = R
    Exit Function
EH:
    M2639 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2640() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Space(Len(s))))
    M2640 = R
    Exit Function
EH:
    M2640 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2641() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Space(Len(s))))
    M2641 = R
    Exit Function
EH:
    M2641 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2642() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(String(2, s)))
    M2642 = R
    Exit Function
EH:
    M2642 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2643() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(String(2, s)))
    M2643 = R
    Exit Function
EH:
    M2643 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2644() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(String(2, s)))
    M2644 = R
    Exit Function
EH:
    M2644 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2645() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(String(2, s)))
    M2645 = R
    Exit Function
EH:
    M2645 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2646() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(String(2, s)))
    M2646 = R
    Exit Function
EH:
    M2646 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2647() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(String(2, s)))
    M2647 = R
    Exit Function
EH:
    M2647 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2648() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(String(2, s)))
    M2648 = R
    Exit Function
EH:
    M2648 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2649() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(String(2, s)))
    M2649 = R
    Exit Function
EH:
    M2649 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2650() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(String(2, s)))
    M2650 = R
    Exit Function
EH:
    M2650 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2651() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(String(2, s)))
    M2651 = R
    Exit Function
EH:
    M2651 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2652() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(String(2, s)))
    M2652 = R
    Exit Function
EH:
    M2652 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2653() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(String(2, s)))
    M2653 = R
    Exit Function
EH:
    M2653 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2654() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(String(2, s)))
    M2654 = R
    Exit Function
EH:
    M2654 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2655() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(String(2, s)))
    M2655 = R
    Exit Function
EH:
    M2655 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2656() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(String(2, s)))
    M2656 = R
    Exit Function
EH:
    M2656 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2657() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(String(2, s)))
    M2657 = R
    Exit Function
EH:
    M2657 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2658() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(String(2, s)))
    M2658 = R
    Exit Function
EH:
    M2658 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2659() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(String(2, s)))
    M2659 = R
    Exit Function
EH:
    M2659 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2660() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(String(2, s)))
    M2660 = R
    Exit Function
EH:
    M2660 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2661() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(String(2, s)))
    M2661 = R
    Exit Function
EH:
    M2661 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2662() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Chr(Asc(s))))
    M2662 = R
    Exit Function
EH:
    M2662 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2663() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Chr(Asc(s))))
    M2663 = R
    Exit Function
EH:
    M2663 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2664() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Chr(Asc(s))))
    M2664 = R
    Exit Function
EH:
    M2664 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2665() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Chr(Asc(s))))
    M2665 = R
    Exit Function
EH:
    M2665 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2666() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Chr(Asc(s))))
    M2666 = R
    Exit Function
EH:
    M2666 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2667() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Chr(Asc(s))))
    M2667 = R
    Exit Function
EH:
    M2667 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2668() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Chr(Asc(s))))
    M2668 = R
    Exit Function
EH:
    M2668 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2669() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Chr(Asc(s))))
    M2669 = R
    Exit Function
EH:
    M2669 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2670() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Chr(Asc(s))))
    M2670 = R
    Exit Function
EH:
    M2670 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2671() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Chr(Asc(s))))
    M2671 = R
    Exit Function
EH:
    M2671 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2672() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Chr(Asc(s))))
    M2672 = R
    Exit Function
EH:
    M2672 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2673() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Chr(Asc(s))))
    M2673 = R
    Exit Function
EH:
    M2673 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2674() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Chr(Asc(s))))
    M2674 = R
    Exit Function
EH:
    M2674 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2675() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Chr(Asc(s))))
    M2675 = R
    Exit Function
EH:
    M2675 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2676() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Chr(Asc(s))))
    M2676 = R
    Exit Function
EH:
    M2676 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2677() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Chr(Asc(s))))
    M2677 = R
    Exit Function
EH:
    M2677 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2678() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Chr(Asc(s))))
    M2678 = R
    Exit Function
EH:
    M2678 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2679() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Chr(Asc(s))))
    M2679 = R
    Exit Function
EH:
    M2679 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2680() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Chr(Asc(s))))
    M2680 = R
    Exit Function
EH:
    M2680 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2681() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Chr(Asc(s))))
    M2681 = R
    Exit Function
EH:
    M2681 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2682() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(ChrW(AscW(s))))
    M2682 = R
    Exit Function
EH:
    M2682 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2683() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(ChrW(AscW(s))))
    M2683 = R
    Exit Function
EH:
    M2683 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2684() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(ChrW(AscW(s))))
    M2684 = R
    Exit Function
EH:
    M2684 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2685() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(ChrW(AscW(s))))
    M2685 = R
    Exit Function
EH:
    M2685 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2686() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(ChrW(AscW(s))))
    M2686 = R
    Exit Function
EH:
    M2686 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2687() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(ChrW(AscW(s))))
    M2687 = R
    Exit Function
EH:
    M2687 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2688() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(ChrW(AscW(s))))
    M2688 = R
    Exit Function
EH:
    M2688 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2689() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(ChrW(AscW(s))))
    M2689 = R
    Exit Function
EH:
    M2689 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2690() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(ChrW(AscW(s))))
    M2690 = R
    Exit Function
EH:
    M2690 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2691() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(ChrW(AscW(s))))
    M2691 = R
    Exit Function
EH:
    M2691 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2692() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(ChrW(AscW(s))))
    M2692 = R
    Exit Function
EH:
    M2692 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2693() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(ChrW(AscW(s))))
    M2693 = R
    Exit Function
EH:
    M2693 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2694() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(ChrW(AscW(s))))
    M2694 = R
    Exit Function
EH:
    M2694 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2695() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(ChrW(AscW(s))))
    M2695 = R
    Exit Function
EH:
    M2695 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2696() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(ChrW(AscW(s))))
    M2696 = R
    Exit Function
EH:
    M2696 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2697() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(ChrW(AscW(s))))
    M2697 = R
    Exit Function
EH:
    M2697 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2698() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(ChrW(AscW(s))))
    M2698 = R
    Exit Function
EH:
    M2698 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2699() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(ChrW(AscW(s))))
    M2699 = R
    Exit Function
EH:
    M2699 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2700() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(ChrW(AscW(s))))
    M2700 = R
    Exit Function
EH:
    M2700 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2701() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(ChrW(AscW(s))))
    M2701 = R
    Exit Function
EH:
    M2701 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2702() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStr(s, "b")))
    M2702 = R
    Exit Function
EH:
    M2702 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2703() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStr(s, "b")))
    M2703 = R
    Exit Function
EH:
    M2703 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2704() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStr(s, "b")))
    M2704 = R
    Exit Function
EH:
    M2704 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2705() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStr(s, "b")))
    M2705 = R
    Exit Function
EH:
    M2705 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2706() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStr(s, "b")))
    M2706 = R
    Exit Function
EH:
    M2706 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2707() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStr(s, "b")))
    M2707 = R
    Exit Function
EH:
    M2707 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2708() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStr(s, "b")))
    M2708 = R
    Exit Function
EH:
    M2708 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2709() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStr(s, "b")))
    M2709 = R
    Exit Function
EH:
    M2709 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2710() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStr(s, "b")))
    M2710 = R
    Exit Function
EH:
    M2710 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2711() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStr(s, "b")))
    M2711 = R
    Exit Function
EH:
    M2711 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2712() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStr(s, "b")))
    M2712 = R
    Exit Function
EH:
    M2712 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2713() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStr(s, "b")))
    M2713 = R
    Exit Function
EH:
    M2713 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2714() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStr(s, "b")))
    M2714 = R
    Exit Function
EH:
    M2714 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2715() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStr(s, "b")))
    M2715 = R
    Exit Function
EH:
    M2715 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2716() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStr(s, "b")))
    M2716 = R
    Exit Function
EH:
    M2716 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2717() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStr(s, "b")))
    M2717 = R
    Exit Function
EH:
    M2717 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2718() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStr(s, "b")))
    M2718 = R
    Exit Function
EH:
    M2718 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2719() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStr(s, "b")))
    M2719 = R
    Exit Function
EH:
    M2719 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2720() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStr(s, "b")))
    M2720 = R
    Exit Function
EH:
    M2720 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2721() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStr(s, "b")))
    M2721 = R
    Exit Function
EH:
    M2721 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2722() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2722 = R
    Exit Function
EH:
    M2722 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2723() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2723 = R
    Exit Function
EH:
    M2723 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2724() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2724 = R
    Exit Function
EH:
    M2724 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2725() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2725 = R
    Exit Function
EH:
    M2725 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2726() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2726 = R
    Exit Function
EH:
    M2726 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2727() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2727 = R
    Exit Function
EH:
    M2727 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2728() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2728 = R
    Exit Function
EH:
    M2728 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2729() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2729 = R
    Exit Function
EH:
    M2729 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2730() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2730 = R
    Exit Function
EH:
    M2730 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2731() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2731 = R
    Exit Function
EH:
    M2731 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2732() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2732 = R
    Exit Function
EH:
    M2732 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2733() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2733 = R
    Exit Function
EH:
    M2733 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2734() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2734 = R
    Exit Function
EH:
    M2734 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2735() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2735 = R
    Exit Function
EH:
    M2735 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2736() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2736 = R
    Exit Function
EH:
    M2736 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2737() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2737 = R
    Exit Function
EH:
    M2737 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2738() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2738 = R
    Exit Function
EH:
    M2738 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2739() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2739 = R
    Exit Function
EH:
    M2739 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2740() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2740 = R
    Exit Function
EH:
    M2740 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2741() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStr(1, s, "B", 1)))
    M2741 = R
    Exit Function
EH:
    M2741 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2742() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "b")))
    M2742 = R
    Exit Function
EH:
    M2742 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2743() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "b")))
    M2743 = R
    Exit Function
EH:
    M2743 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2744() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "b")))
    M2744 = R
    Exit Function
EH:
    M2744 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2745() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "b")))
    M2745 = R
    Exit Function
EH:
    M2745 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2746() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "b")))
    M2746 = R
    Exit Function
EH:
    M2746 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2747() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "b")))
    M2747 = R
    Exit Function
EH:
    M2747 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2748() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "b")))
    M2748 = R
    Exit Function
EH:
    M2748 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2749() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "b")))
    M2749 = R
    Exit Function
EH:
    M2749 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2750() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "b")))
    M2750 = R
    Exit Function
EH:
    M2750 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2751() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "b")))
    M2751 = R
    Exit Function
EH:
    M2751 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2752() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "b")))
    M2752 = R
    Exit Function
EH:
    M2752 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2753() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "b")))
    M2753 = R
    Exit Function
EH:
    M2753 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2754() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "b")))
    M2754 = R
    Exit Function
EH:
    M2754 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2755() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "b")))
    M2755 = R
    Exit Function
EH:
    M2755 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2756() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "b")))
    M2756 = R
    Exit Function
EH:
    M2756 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2757() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "b")))
    M2757 = R
    Exit Function
EH:
    M2757 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2758() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "b")))
    M2758 = R
    Exit Function
EH:
    M2758 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2759() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "b")))
    M2759 = R
    Exit Function
EH:
    M2759 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2760() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "b")))
    M2760 = R
    Exit Function
EH:
    M2760 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2761() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "b")))
    M2761 = R
    Exit Function
EH:
    M2761 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2762() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2762 = R
    Exit Function
EH:
    M2762 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2763() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2763 = R
    Exit Function
EH:
    M2763 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2764() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2764 = R
    Exit Function
EH:
    M2764 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2765() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2765 = R
    Exit Function
EH:
    M2765 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2766() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2766 = R
    Exit Function
EH:
    M2766 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2767() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2767 = R
    Exit Function
EH:
    M2767 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2768() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2768 = R
    Exit Function
EH:
    M2768 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2769() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2769 = R
    Exit Function
EH:
    M2769 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2770() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2770 = R
    Exit Function
EH:
    M2770 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2771() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2771 = R
    Exit Function
EH:
    M2771 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2772() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2772 = R
    Exit Function
EH:
    M2772 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2773() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2773 = R
    Exit Function
EH:
    M2773 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2774() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2774 = R
    Exit Function
EH:
    M2774 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2775() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2775 = R
    Exit Function
EH:
    M2775 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2776() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2776 = R
    Exit Function
EH:
    M2776 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2777() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2777 = R
    Exit Function
EH:
    M2777 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2778() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2778 = R
    Exit Function
EH:
    M2778 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2779() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2779 = R
    Exit Function
EH:
    M2779 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2780() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2780 = R
    Exit Function
EH:
    M2780 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2781() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "B", -1, 1)))
    M2781 = R
    Exit Function
EH:
    M2781 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2782() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "", -1)))
    M2782 = R
    Exit Function
EH:
    M2782 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2783() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2783 = R
    Exit Function
EH:
    M2783 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2784() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2784 = R
    Exit Function
EH:
    M2784 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2785() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2785 = R
    Exit Function
EH:
    M2785 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2786() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "", -1)))
    M2786 = R
    Exit Function
EH:
    M2786 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2787() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2787 = R
    Exit Function
EH:
    M2787 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2788() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2788 = R
    Exit Function
EH:
    M2788 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2789() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2789 = R
    Exit Function
EH:
    M2789 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2790() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2790 = R
    Exit Function
EH:
    M2790 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2791() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2791 = R
    Exit Function
EH:
    M2791 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2792() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "", -1)))
    M2792 = R
    Exit Function
EH:
    M2792 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2793() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2793 = R
    Exit Function
EH:
    M2793 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2794() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2794 = R
    Exit Function
EH:
    M2794 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2795() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2795 = R
    Exit Function
EH:
    M2795 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2796() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "", -1)))
    M2796 = R
    Exit Function
EH:
    M2796 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2797() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "", -1)))
    M2797 = R
    Exit Function
EH:
    M2797 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2798() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2798 = R
    Exit Function
EH:
    M2798 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2799() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2799 = R
    Exit Function
EH:
    M2799 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2800() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2800 = R
    Exit Function
EH:
    M2800 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2801() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "", -1)))
    M2801 = R
    Exit Function
EH:
    M2801 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2802() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2802 = R
    Exit Function
EH:
    M2802 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2803() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2803 = R
    Exit Function
EH:
    M2803 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2804() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2804 = R
    Exit Function
EH:
    M2804 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2805() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2805 = R
    Exit Function
EH:
    M2805 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2806() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2806 = R
    Exit Function
EH:
    M2806 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2807() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2807 = R
    Exit Function
EH:
    M2807 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2808() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2808 = R
    Exit Function
EH:
    M2808 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2809() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2809 = R
    Exit Function
EH:
    M2809 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2810() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2810 = R
    Exit Function
EH:
    M2810 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2811() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2811 = R
    Exit Function
EH:
    M2811 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2812() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2812 = R
    Exit Function
EH:
    M2812 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2813() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2813 = R
    Exit Function
EH:
    M2813 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2814() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2814 = R
    Exit Function
EH:
    M2814 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2815() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2815 = R
    Exit Function
EH:
    M2815 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2816() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2816 = R
    Exit Function
EH:
    M2816 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2817() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2817 = R
    Exit Function
EH:
    M2817 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2818() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2818 = R
    Exit Function
EH:
    M2818 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2819() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2819 = R
    Exit Function
EH:
    M2819 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2820() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2820 = R
    Exit Function
EH:
    M2820 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2821() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "b", 0)))
    M2821 = R
    Exit Function
EH:
    M2821 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2822() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2822 = R
    Exit Function
EH:
    M2822 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2823() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2823 = R
    Exit Function
EH:
    M2823 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2824() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2824 = R
    Exit Function
EH:
    M2824 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2825() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2825 = R
    Exit Function
EH:
    M2825 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2826() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2826 = R
    Exit Function
EH:
    M2826 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2827() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2827 = R
    Exit Function
EH:
    M2827 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2828() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2828 = R
    Exit Function
EH:
    M2828 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2829() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2829 = R
    Exit Function
EH:
    M2829 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2830() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2830 = R
    Exit Function
EH:
    M2830 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2831() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2831 = R
    Exit Function
EH:
    M2831 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2832() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2832 = R
    Exit Function
EH:
    M2832 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2833() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2833 = R
    Exit Function
EH:
    M2833 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2834() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2834 = R
    Exit Function
EH:
    M2834 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2835() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2835 = R
    Exit Function
EH:
    M2835 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2836() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2836 = R
    Exit Function
EH:
    M2836 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2837() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2837 = R
    Exit Function
EH:
    M2837 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2838() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2838 = R
    Exit Function
EH:
    M2838 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2839() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2839 = R
    Exit Function
EH:
    M2839 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2840() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2840 = R
    Exit Function
EH:
    M2840 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2841() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "b", 2)))
    M2841 = R
    Exit Function
EH:
    M2841 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2842() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2842 = R
    Exit Function
EH:
    M2842 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2843() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2843 = R
    Exit Function
EH:
    M2843 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2844() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2844 = R
    Exit Function
EH:
    M2844 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2845() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2845 = R
    Exit Function
EH:
    M2845 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2846() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2846 = R
    Exit Function
EH:
    M2846 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2847() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2847 = R
    Exit Function
EH:
    M2847 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2848() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2848 = R
    Exit Function
EH:
    M2848 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2849() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2849 = R
    Exit Function
EH:
    M2849 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2850() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2850 = R
    Exit Function
EH:
    M2850 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2851() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2851 = R
    Exit Function
EH:
    M2851 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2852() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2852 = R
    Exit Function
EH:
    M2852 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2853() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2853 = R
    Exit Function
EH:
    M2853 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2854() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2854 = R
    Exit Function
EH:
    M2854 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2855() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2855 = R
    Exit Function
EH:
    M2855 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2856() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2856 = R
    Exit Function
EH:
    M2856 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2857() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2857 = R
    Exit Function
EH:
    M2857 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2858() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2858 = R
    Exit Function
EH:
    M2858 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2859() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2859 = R
    Exit Function
EH:
    M2859 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2860() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2860 = R
    Exit Function
EH:
    M2860 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2861() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(InStrRev(s, "b", 100)))
    M2861 = R
    Exit Function
EH:
    M2861 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2862() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X")))
    M2862 = R
    Exit Function
EH:
    M2862 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2863() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X")))
    M2863 = R
    Exit Function
EH:
    M2863 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2864() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X")))
    M2864 = R
    Exit Function
EH:
    M2864 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2865() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X")))
    M2865 = R
    Exit Function
EH:
    M2865 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2866() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X")))
    M2866 = R
    Exit Function
EH:
    M2866 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2867() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X")))
    M2867 = R
    Exit Function
EH:
    M2867 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2868() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X")))
    M2868 = R
    Exit Function
EH:
    M2868 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2869() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X")))
    M2869 = R
    Exit Function
EH:
    M2869 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2870() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X")))
    M2870 = R
    Exit Function
EH:
    M2870 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2871() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X")))
    M2871 = R
    Exit Function
EH:
    M2871 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2872() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X")))
    M2872 = R
    Exit Function
EH:
    M2872 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2873() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X")))
    M2873 = R
    Exit Function
EH:
    M2873 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2874() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X")))
    M2874 = R
    Exit Function
EH:
    M2874 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2875() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X")))
    M2875 = R
    Exit Function
EH:
    M2875 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2876() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X")))
    M2876 = R
    Exit Function
EH:
    M2876 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2877() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X")))
    M2877 = R
    Exit Function
EH:
    M2877 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2878() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X")))
    M2878 = R
    Exit Function
EH:
    M2878 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2879() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X")))
    M2879 = R
    Exit Function
EH:
    M2879 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2880() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X")))
    M2880 = R
    Exit Function
EH:
    M2880 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2881() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X")))
    M2881 = R
    Exit Function
EH:
    M2881 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2882() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2882 = R
    Exit Function
EH:
    M2882 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2883() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2883 = R
    Exit Function
EH:
    M2883 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2884() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2884 = R
    Exit Function
EH:
    M2884 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2885() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2885 = R
    Exit Function
EH:
    M2885 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2886() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2886 = R
    Exit Function
EH:
    M2886 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2887() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2887 = R
    Exit Function
EH:
    M2887 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2888() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2888 = R
    Exit Function
EH:
    M2888 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2889() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2889 = R
    Exit Function
EH:
    M2889 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2890() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2890 = R
    Exit Function
EH:
    M2890 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2891() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2891 = R
    Exit Function
EH:
    M2891 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2892() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2892 = R
    Exit Function
EH:
    M2892 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2893() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2893 = R
    Exit Function
EH:
    M2893 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2894() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2894 = R
    Exit Function
EH:
    M2894 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2895() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2895 = R
    Exit Function
EH:
    M2895 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2896() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2896 = R
    Exit Function
EH:
    M2896 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2897() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2897 = R
    Exit Function
EH:
    M2897 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2898() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2898 = R
    Exit Function
EH:
    M2898 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2899() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2899 = R
    Exit Function
EH:
    M2899 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2900() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2900 = R
    Exit Function
EH:
    M2900 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2901() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "B", "X", , , 1)))
    M2901 = R
    Exit Function
EH:
    M2901 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2902() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "", "X")))
    M2902 = R
    Exit Function
EH:
    M2902 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2903() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "", "X")))
    M2903 = R
    Exit Function
EH:
    M2903 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2904() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "", "X")))
    M2904 = R
    Exit Function
EH:
    M2904 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2905() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "", "X")))
    M2905 = R
    Exit Function
EH:
    M2905 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2906() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "", "X")))
    M2906 = R
    Exit Function
EH:
    M2906 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2907() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "", "X")))
    M2907 = R
    Exit Function
EH:
    M2907 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2908() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "", "X")))
    M2908 = R
    Exit Function
EH:
    M2908 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2909() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "", "X")))
    M2909 = R
    Exit Function
EH:
    M2909 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2910() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "", "X")))
    M2910 = R
    Exit Function
EH:
    M2910 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2911() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "", "X")))
    M2911 = R
    Exit Function
EH:
    M2911 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2912() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "", "X")))
    M2912 = R
    Exit Function
EH:
    M2912 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2913() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "", "X")))
    M2913 = R
    Exit Function
EH:
    M2913 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2914() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "", "X")))
    M2914 = R
    Exit Function
EH:
    M2914 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2915() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "", "X")))
    M2915 = R
    Exit Function
EH:
    M2915 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2916() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "", "X")))
    M2916 = R
    Exit Function
EH:
    M2916 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2917() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "", "X")))
    M2917 = R
    Exit Function
EH:
    M2917 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2918() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "", "X")))
    M2918 = R
    Exit Function
EH:
    M2918 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2919() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "", "X")))
    M2919 = R
    Exit Function
EH:
    M2919 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2920() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "", "X")))
    M2920 = R
    Exit Function
EH:
    M2920 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2921() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "", "X")))
    M2921 = R
    Exit Function
EH:
    M2921 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2922() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "")))
    M2922 = R
    Exit Function
EH:
    M2922 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2923() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "")))
    M2923 = R
    Exit Function
EH:
    M2923 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2924() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "")))
    M2924 = R
    Exit Function
EH:
    M2924 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2925() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "")))
    M2925 = R
    Exit Function
EH:
    M2925 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2926() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "")))
    M2926 = R
    Exit Function
EH:
    M2926 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2927() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "")))
    M2927 = R
    Exit Function
EH:
    M2927 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2928() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "")))
    M2928 = R
    Exit Function
EH:
    M2928 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2929() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "")))
    M2929 = R
    Exit Function
EH:
    M2929 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2930() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "")))
    M2930 = R
    Exit Function
EH:
    M2930 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2931() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "")))
    M2931 = R
    Exit Function
EH:
    M2931 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2932() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "")))
    M2932 = R
    Exit Function
EH:
    M2932 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2933() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "")))
    M2933 = R
    Exit Function
EH:
    M2933 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2934() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "")))
    M2934 = R
    Exit Function
EH:
    M2934 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2935() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "")))
    M2935 = R
    Exit Function
EH:
    M2935 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2936() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "")))
    M2936 = R
    Exit Function
EH:
    M2936 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2937() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "")))
    M2937 = R
    Exit Function
EH:
    M2937 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2938() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "")))
    M2938 = R
    Exit Function
EH:
    M2938 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2939() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "")))
    M2939 = R
    Exit Function
EH:
    M2939 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2940() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "")))
    M2940 = R
    Exit Function
EH:
    M2940 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2941() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "")))
    M2941 = R
    Exit Function
EH:
    M2941 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2942() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2942 = R
    Exit Function
EH:
    M2942 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2943() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2943 = R
    Exit Function
EH:
    M2943 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2944() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2944 = R
    Exit Function
EH:
    M2944 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2945() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2945 = R
    Exit Function
EH:
    M2945 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2946() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2946 = R
    Exit Function
EH:
    M2946 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2947() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2947 = R
    Exit Function
EH:
    M2947 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2948() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2948 = R
    Exit Function
EH:
    M2948 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2949() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2949 = R
    Exit Function
EH:
    M2949 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2950() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2950 = R
    Exit Function
EH:
    M2950 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2951() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2951 = R
    Exit Function
EH:
    M2951 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2952() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2952 = R
    Exit Function
EH:
    M2952 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2953() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2953 = R
    Exit Function
EH:
    M2953 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2954() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2954 = R
    Exit Function
EH:
    M2954 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2955() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2955 = R
    Exit Function
EH:
    M2955 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2956() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2956 = R
    Exit Function
EH:
    M2956 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2957() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2957 = R
    Exit Function
EH:
    M2957 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2958() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2958 = R
    Exit Function
EH:
    M2958 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2959() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2959 = R
    Exit Function
EH:
    M2959 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2960() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2960 = R
    Exit Function
EH:
    M2960 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2961() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "XX", 2)))
    M2961 = R
    Exit Function
EH:
    M2961 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2962() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2962 = R
    Exit Function
EH:
    M2962 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2963() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2963 = R
    Exit Function
EH:
    M2963 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2964() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2964 = R
    Exit Function
EH:
    M2964 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2965() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2965 = R
    Exit Function
EH:
    M2965 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2966() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2966 = R
    Exit Function
EH:
    M2966 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2967() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2967 = R
    Exit Function
EH:
    M2967 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2968() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2968 = R
    Exit Function
EH:
    M2968 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2969() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2969 = R
    Exit Function
EH:
    M2969 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2970() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2970 = R
    Exit Function
EH:
    M2970 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2971() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2971 = R
    Exit Function
EH:
    M2971 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2972() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2972 = R
    Exit Function
EH:
    M2972 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2973() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2973 = R
    Exit Function
EH:
    M2973 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2974() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2974 = R
    Exit Function
EH:
    M2974 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2975() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2975 = R
    Exit Function
EH:
    M2975 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2976() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2976 = R
    Exit Function
EH:
    M2976 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2977() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2977 = R
    Exit Function
EH:
    M2977 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2978() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2978 = R
    Exit Function
EH:
    M2978 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2979() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2979 = R
    Exit Function
EH:
    M2979 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2980() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "tab" & vbTab & "x"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2980 = R
    Exit Function
EH:
    M2980 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2981() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "1" & ChrW(160) & "2"
    R = Dump(fV(Replace(s, "b", "X", -1, 1)))
    M2981 = R
    Exit Function
EH:
    M2981 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2982() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ""
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2982 = R
    Exit Function
EH:
    M2982 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2983() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "abc"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2983 = R
    Exit Function
EH:
    M2983 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2984() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "ABC"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2984 = R
    Exit Function
EH:
    M2984 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2985() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "aBc"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2985 = R
    Exit Function
EH:
    M2985 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2986() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "  a b  "
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2986 = R
    Exit Function
EH:
    M2986 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2987() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a,b,,c"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2987 = R
    Exit Function
EH:
    M2987 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2988() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(0) & "b"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2988 = R
    Exit Function
EH:
    M2988 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2989() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "a" & ChrW(233) & "B"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2989 = R
    Exit Function
EH:
    M2989 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2990() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "A" & ChrW(201) & "b"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2990 = R
    Exit Function
EH:
    M2990 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2991() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HD83D) & ChrW(&HDE00) & "x"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2991 = R
    Exit Function
EH:
    M2991 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2992() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H3A3) & ChrW(&H3C3) & ChrW(&H3C2)
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2992 = R
    Exit Function
EH:
    M2992 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2993() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "stra" & ChrW(223) & "e"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2993 = R
    Exit Function
EH:
    M2993 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2994() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H130) & "i"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2994 = R
    Exit Function
EH:
    M2994 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2995() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H131) & "I"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2995 = R
    Exit Function
EH:
    M2995 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2996() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&HFF21) & ChrW(&HFF41)
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2996 = R
    Exit Function
EH:
    M2996 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2997() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = ChrW(&H1C5)
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2997 = R
    Exit Function
EH:
    M2997 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2998() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "hello world foo"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2998 = R
    Exit Function
EH:
    M2998 = "#ERR " & Err.Number & " " & Err.Description
End Function

Private Function M2999() As Variant
    On Error GoTo EH
    Dim R As Variant
    Dim s As String
    s = "line1" & vbCrLf & "line2"
    R = Dump(fV(Replace(s, "b", "X", 1, 0)))
    M2999 = R
    Exit Function
EH:
    M2999 = "#ERR " & Err.Number & " " & Err.Description
End Function
