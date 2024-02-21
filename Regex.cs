using System.Text;

namespace ByteRegularExpression
{
    public class Regex
    {
        public RegexArg[] args;
        public Regex(string regex)
        {
            this.args = ParseFullBlock(regex);
        }

        static RegexArg[] ParseFullBlock(string regex)
        {
            int i = 0;
            List<RegexArg> args = new List<RegexArg>();
            while(i < regex.Length)
            {
                args.Add(ParseSingleBlock(regex,ref i));
            }
            return args.ToArray();
        }

        static char[] ParseString(string regex, ref int i)
        {
            if (regex[i++] != '\"') throw new Exception();
            List<char> charlist = new List<char>();
            while(i< regex.Length)
            {
                char c = regex[i++];
                if (c == '\"')
                    return charlist.ToArray();
                charlist.Add(c);
            }
            throw new Exception();
        }
        static byte ParseSingleUnknown(string regex, ref int i)
        {
            switch(regex[i++])
            {
                case 'x':
                    return ParseSingleByte(regex, ref i);
                case 'c':
                    return ParseSingleChar(regex, ref i);
                default: throw new Exception();
            }
        }
        static byte ParseSingleByte(string regex, ref int i)
        {
            return Convert.ToByte($"{regex[i++]}{regex[i++]}", 16);
        }
        static byte ParseSingleChar(string regex, ref int i)
        {
            return Encoding.ASCII.GetBytes(regex, i++, 1)[0];
        }
        static Range ParseCountOf(string regex, ref int i)
        {
            StringBuilder sb = new StringBuilder();
            while (regex[i++] != '}') sb.Append(regex[i - 1]);
            string countResult = sb.ToString();

            if (countResult.Any(x => x == ','))
            {
                string[] countResults = countResult.Split(',');
                switch (countResults.Length)
                {
                    case 1:
                        return int.Parse(countResults[0]) .. int.MaxValue;
                    case 2:
                        return int.Parse(countResults[0]) ..  int.Parse(countResults[1]);
                    default:
                        throw new Exception();
                }
            }
            else
                return int.Parse(countResult) .. int.Parse(countResult);
        }
        static bool[] ParseOrByte(string regex, ref int i)
        {
            bool[] result = Enumerable.Repeat(false, 256).ToArray();
            byte? lastByte = null;
            bool isReverse = false;
            while(true)
            {
                char m = regex[i++];
                switch(m)
                {
                    case 'x':
                        if (lastByte is not null)
                            result[lastByte??0x00] = true;
                        lastByte = ParseSingleByte(regex, ref i);
                        break;
                    case 'c':
                        if (lastByte is not null)
                            result[lastByte ?? 0x00] = true;
                        lastByte = ParseSingleChar(regex, ref i);
                        break;
                    case '-':
                        byte laterByte = ParseSingleUnknown(regex, ref i);
                        byte min = Math.Min(lastByte ?? 0x00, laterByte);
                        byte max = Math.Max(lastByte ?? 0x00, laterByte);
                        for (byte j = min; j <= max; j++)
                            result[j] = true;
                        lastByte = null;
                        break;
                    case '^':
                        isReverse = !isReverse;
                        break;
                    case ']':
                        if (lastByte is not null)
                            result[lastByte ?? 0x00] = true;
                        if (isReverse)
                            for (int j = 0; j < 256; j++) result[j] = !result[j];
                        return result;
                }
            }
        }
        static RegexArg ParseSingleBlock(string regex, ref int i)
        {
            List<List<RegexArg>> argsOrs = [[]];
            int currentOrStep = 0;

            while(true)
            {
                if(i == regex.Length)
                {
                   return Finalize();
                }
                char m = regex[i++];
                switch(m)
                {
                    case 'x':
                        argsOrs[currentOrStep].Add(new RegexArg.OrByte(ParseSingleByte(regex, ref i)));
                        break;
                    case 'c':
                        argsOrs[currentOrStep].Add(new RegexArg.OrByte(ParseSingleChar(regex, ref i)));
                        break;
                    case '.':
                        argsOrs[currentOrStep].Add(new RegexArg.OrByte(Enumerable.Repeat(true,256).ToArray()));
                        break;

                    case 's':
                        foreach(byte c in ParseString(regex, ref i))
                        {
                            argsOrs[currentOrStep].Add(new RegexArg.OrByte(c));
                        }
                        break;
                    case 'h':
                        char[] chars = ParseString(regex, ref i);
                        if (chars.Length % 2 == 1) throw new Exception();
                        for(int j=0;j<chars.Length;j+=2)
                        {
                            argsOrs[currentOrStep].Add(new RegexArg.OrByte(Convert.ToByte($"{chars[j]}{chars[j+1]}",16)));
                        }
                        break;

                    case '[':
                        argsOrs[currentOrStep].Add(new RegexArg.OrByte(ParseOrByte(regex, ref i)));
                        break;
                    case '{':
                    openCountOf:
                        Range range = ParseCountOf(regex, ref i);
                        return  new RegexArg.CountOf(Finalize(), range);
                    case '|':
                        currentOrStep++;
                        break;
                    case '(':
                        return Finalize();
                    case ')':
                        if (i == regex.Length) return Finalize();
                        switch(regex[i])
                        {
                            case '{' or '+':
                                i++;
                                goto openCountOf;
                            default:
                                return Finalize();
                        }
                }
            }
            
            RegexArg Finalize() => new RegexArg.GroupOrBytes(argsOrs.Select(x => new RegexArg.GroupBytes(x.ToArray())).ToArray());
        }
        enum ParseState
        {
            Basic,
            OrState,
            OrStateRangeEnd,
        }


        public int MatchAt(byte[] bytes, int index)
        {
            int startindex = index;
            int i = index;
            foreach(var arg in args)
            {
                if(!arg.IsMatch(bytes, ref i))
                {
                    return 0;
                }
            }
            return i -  startindex;
        }
        public Range[] MatchAll(byte[] bytes, int index, int length)
        {
            List<Range> ranges = new List<Range>();
            for(int i = index; i < index + length;)
            {
                int matchLen = MatchAt(bytes, i);
                if(matchLen != 0)
                {
                    ranges.Add(i..(i+ matchLen));
                    i += matchLen; //  no shifted duplicate
                }
                else
                {
                    i++;
                }
            }
            return ranges.ToArray();
        }
        public void CompileCSMatchAt()
        {
            //DynamicMethod method = new DynamicMethod("CompiledRegex", typeof((int index, int count)[]), new Type[] { typeof(byte[]), typeof(int), typeof(int) });
            //gg
            throw new Exception();
        }
    }
    public interface RegexArg
    {
        public interface GroupWorking;
        public bool IsMatch(byte[] bytes, ref int i);


        public class OrByte : RegexArg
        {
            public OrByte(bool[] adresses)
            {
                this.adresses = adresses;
            }
            public OrByte(params byte[] trueBytes)
            {
                this.adresses = Enumerable.Repeat(false, 256).ToArray();
                foreach(byte b in trueBytes)
                {
                    adresses[b] = true;
                }
            }
            bool[] adresses { get; init; }

            public bool IsMatch(byte[] bytes, ref int i)
            {
                if (adresses[bytes[i]])
                {
                    i++;
                    return true;
                }
                return false;
            }
        }
        public class GroupBytes : RegexArg, GroupWorking
        {
            public GroupBytes(RegexArg[] args)
            {
                this.args = args;
            }
            RegexArg[] args { get; init; }

            public bool IsMatch(byte[] bytes, ref int i)
            {
                int startindex = i;
                try
                {
                    foreach (var arg in args)
                    {
                        if (!arg.IsMatch(bytes, ref i))
                        {
                            i = startindex;
                            return false;
                        }
                    }
                    return true;
                }
                catch
                {
                    i = startindex;
                    return false;
                }
            }
        }

        public class GroupOrBytes : RegexArg, GroupWorking
        {
            public GroupOrBytes(GroupBytes[] orGroups)
            {
                this.orGroups = orGroups;
            }
            GroupBytes[] orGroups { get; init; }

            public bool IsMatch(byte[] bytes, ref int i)
            {
                try
                {
                    foreach (var orGroup in orGroups)
                    {
                        if (orGroup.IsMatch(bytes, ref i))
                            return true;
                    }
                    return false;
                }
                catch
                {
                    return false;
                }
            }
        }
        public class CountOf : RegexArg, GroupWorking
        {
            public CountOf(RegexArg arg, Range range)
            {
                this.arg = arg;
                this.range = range;
            }


            RegexArg arg { get; init; }
            Range range { get; init; }
            public bool IsMatch(byte[] bytes, ref int i)
            {
                int startindex = i;
                int count = 0;
                while (true)
                    if (arg.IsMatch(bytes, ref i))
                    {
                        if (count == range.End.Value-1)
                            return true;
                        count++;
                    }
                    else if (range.Start.Value <= count && count < range.End.Value)
                        return true;
                    else
                    {
                        i = startindex;
                        return false;
                    }
            }
        }

    }
}
