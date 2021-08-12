-- INPUT
DECLARE @input NVARCHAR(MAX) = '<font><i>If you have a Spark NV, Maiden NC, or Dallas TX location: <br>To comply with Apple security protocols, do not identify your location as an Apple Data Center in your job title.</i></br>'

-- 
DECLARE @Nodes TABLE (Id INT identity(1,1) primary key, Name NVARCHAR(50), Text NVARCHAR(MAX) DEFAULT NULL, Integrity bit DEFAULT 0)
DECLARE @Attributes TABLE (Id INT identity(1,1) primary key, NodeId int, Name NVARCHAR(50), [Value] NVARCHAR(500))
DECLARE @Styles TABLE (Id INT identity(1,1) primary key, NodeId int, Name NVARCHAR(50), [Value] NVARCHAR(50))

---------
DECLARE @SingleNodes NVARCHAR(MAX) = '|br|hr|'

DECLARE @ERR_NODE_NOT_MATCH NVARCHAR(120) = 'The format of document is not correct. Please check the following node(s) in the document, which is/are {0}.'

--------------------------------------------------------------------------------
DECLARE @startIndex INT = CHARINDEX('<', @input),
        @endIndex INT = 0,
        @nextStartIndex INT = 0,
        @nodeDesc NVARCHAR(MAX) = '',
        @node NVARCHAR(20) = '',
        @nodePath NVARCHAR(MAX) = '',
        @text NVARCHAR(MAX) = '',
        @foundNodeId INT = 0,
        @attributeIndex INT = 0,
        @attributeInfo NVARCHAR(MAX) = '',
        @attribute  NVARCHAR(20) = '',
        @attributeValue NVARCHAR(500) = '',
        @styleIndex INT = 0,
        @styleName  NVARCHAR(20) = '',
        @styleValue NVARCHAR(100) = '',
        @quote NVARCHAR(1) = ''
IF @startIndex > 0
BEGIN
    SET @text = SUBSTRING(@input, 1, @startIndex - 1)
    SET @input = SUBSTRING(@input, @startIndex, LEN(@input) - LEN(@text))

    IF ISNULL(@text, '') <> '' 
    BEGIN
        INSERT INTO @Nodes (Name, Text, Integrity) VALUES ('#text', @text, 1);
        SET @text = ''
    END

    SET @startIndex = 1
    WHILE(@startIndex > 0)
    BEGIN   --WHILE
        SET @endIndex = CHARINDEX('>', @input)
        SET @nextStartIndex = CHARINDEX('<', SUBSTRING(@input, @startIndex + 1, LEN(@input) - @startIndex))
        
        IF @nextStartIndex > 0 and @nextStartIndex < @endIndex and @endIndex > 0
        BEGIN		
            SET @text = CONCAT(@text, SUBSTRING(@input, 1, @nextStartIndex))
            SET @input = SUBSTRING(@input, @nextStartIndex, LEN(@input) - @nextStartIndex)
            SET @startIndex = 1
        END
        ELSE
        BEGIN
            IF ISNULL(@text, '') <> '' 
            BEGIN
                INSERT INTO @Nodes (Name, Text, Integrity) VALUES ('#text', @text, 1);
                SET @text = ''
            END
            IF @endIndex > 0
            BEGIN   --IF
                SET @nodeDesc = SUBSTRING(@input, 1, @endIndex)
                
                -- parse the nodes begin 
                IF CHARINDEX(' ', @nodeDesc) > 0
                    SET @node = SUBSTRING(@nodeDesc, 2, CHARINDEX(' ', @nodeDesc) - 1)
                ELSE IF CHARINDEX('/>', @nodeDesc) > 0
                    SET @node = SUBSTRING(@nodeDesc, 2, CHARINDEX('/>', @nodeDesc) - 2)
                ELSE
                    SET @node = SUBSTRING(@nodeDesc, 2, @endIndex - 2)

                Set @node = LTRIM(RTRIM(@node))

                IF CHARINDEX('/', @node) = 1 
                BEGIN
                    SELECT @foundNodeId = Id FROM @Nodes WHERE Name = SUBSTRING(@node, 2, LEN(@node) - 1) ORDER BY Id DESC
                    IF @foundNodeId > 0 
                        Update @Nodes SET Integrity = 1 WHERE Id = @foundNodeId
                    INSERT INTO @Nodes (Name, Integrity) VALUES (@node, 1)
                    
                    IF CHARINDEX('|' + SUBSTRING(@node, 2, LEN(@node) - 1) + '|', @SingleNodes) = 0
                    BEGIN
                        IF CHARINDEX(',', @nodePath) > 0 AND CHARINDEX(SUBSTRING(@node, 2, LEN(@node) - 1), SUBSTRING(@nodePath, LEN(@nodePath) - CHARINDEX(',', REVERSE(@nodePath)) + 2, CHARINDEX(',', REVERSE(@nodePath)) - 1)) = 1 
                            SET @nodePath = SUBSTRING(@nodePath, 1, LEN(@nodePath) - CHARINDEX(',', REVERSE(@nodePath)))
                        ELSE
                            SET @nodePath = CONCAT('[ERROR] ', REPLACE(@ERR_NODE_NOT_MATCH, '{0}', '<' + @node + '>'))
                    END
                END
                ELSE
                BEGIN
                    IF CHARINDEX('|' + @node + '|', @SingleNodes) = 0
                        SET @nodePath = CONCAT(@nodePath, ',', @node)

                    INSERT INTO @Nodes (Name, Integrity) VALUES (@node, CASE WHEN CHARINDEX('|' + @node + '|', @SingleNodes) > 0 THEN 1 ELSE 0 END)
                    SET @foundNodeId = SCOPE_IDENTITY()

                    SET @attributeInfo = LTRIM(RTRIM(REPLACE(REPLACE(@nodeDesc, '<' + @node, ''), '>', '')))
                    -- parse the attributes begin
                    SET @attributeIndex = CHARINDEX('=', @attributeInfo);

                    WHILE @attributeIndex > 0
                    BEGIN
                        SET @attribute = LTRIM(RTRIM(SUBSTRING(@attributeInfo, 1, @attributeIndex - 1)))
                        SET @attributeInfo = SUBSTRING(@attributeInfo, @attributeIndex + 1, LEN(@attributeInfo) - @attributeIndex)

                        DECLARE @singleQuoteIndex int = CHARINDEX('''', @attributeInfo)
                        DECLARE @doubleQuoteIndex int = CHARINDEX('"', @attributeInfo)
                        DECLARE @charIndex int = PATINDEX('%[^ "'']%', @attributeInfo)
                            
                        IF @doubleQuoteIndex > 0 AND @doubleQuoteIndex < @charIndex AND (@doubleQuoteIndex < @singleQuoteIndex OR @singleQuoteIndex = 0)
                        BEGIN
                            SET @attributeInfo = SUBSTRING(@attributeInfo, @doubleQuoteIndex + 1, LEN(@attributeInfo) - @doubleQuoteIndex)
                            DECLARE @doubleQuoteEndIndex INT = CHARINDEX('"', @attributeInfo)
                            SET @attributeValue = LTRIM(RTRIM(SUBSTRING(@attributeInfo, 1, @doubleQuoteEndIndex - 1)))
                            SET @attributeInfo = LTRIM(RTRIM(SUBSTRING(@attributeInfo, @doubleQuoteEndIndex + 1, LEN(@attributeInfo) - @doubleQuoteEndIndex)))
                            SET @attributeIndex = CHARINDEX('=', @attributeInfo);
                        END
                        ELSE IF @singleQuoteIndex > 0 AND @singleQuoteIndex < @charIndex AND (@singleQuoteIndex < @doubleQuoteIndex OR @doubleQuoteIndex = 0)
                        BEGIN
                            SET @attributeInfo = SUBSTRING(@attributeInfo, @singleQuoteIndex + 1, LEN(@attributeInfo) - @singleQuoteIndex)
                            DECLARE @singleQuoteEndIndex INT = CHARINDEX('''', @attributeInfo)
                            SET @attributeValue = LTRIM(RTRIM(SUBSTRING(@attributeInfo, 1, @singleQuoteEndIndex - 1)))
                            SET @attributeInfo = LTRIM(RTRIM(SUBSTRING(@attributeInfo, @singleQuoteEndIndex + 1, LEN(@attributeInfo) - @singleQuoteEndIndex)))
                            SET @attributeIndex = CHARINDEX('=', @attributeInfo);
                        END
                        ELSE
                        BEGIN
                            SET @attributeInfo = LTRIM(RTRIM(SUBSTRING(@attributeInfo, @charIndex, LEN(@attributeInfo) - @charIndex + 1)))
                            SET @attributeIndex = CHARINDEX('=', @attributeInfo);
                            IF @attributeIndex > 0
                            BEGIN
                                SET @attributeValue = LTRIM(RTRIM(SUBSTRING(@attributeInfo, 1, CHARINDEX(' ', @attributeInfo) - 1)))
                                SET @attributeInfo = LTRIM(RTRIM(REPLACE(@attributeInfo, @attributeValue, '')))
                            END
                            ELSE
                                SET @attributeValue = @attributeInfo
                            SET @attributeValue = LTRIM(RTRIM(@attributeValue))
                        END

                        IF @attribute = 'style'
                        BEGIN
                            -- parse the style begin
                            SET @quote = SUBSTRING(@attributeValue, 1, 1)

                            IF @quote = '"' OR @quote = ''''   
                                SET @attributeValue = LTRIM(RTRIM(SUBSTRING(@attributeValue, 2, LEN(@attributeValue) - 2)))

                            IF SUBSTRING(REVERSE(@attributeValue), 1, 1) <> ';'
                                SET @attributeValue = CONCAT(@attributeValue, ';')
                            SET @styleIndex = CHARINDEX(';', @attributeValue);
                            IF @styleIndex = 0
                                SET @styleIndex = LEN(@attributeValue)

                            WHILE @styleIndex > 0
                            BEGIN
                                SET @styleName = LTRIM(RTRIM(SUBSTRING(@attributeValue, 1, CHARINDEX(':', @attributeValue) - 1)))
                                SET @styleValue = LTRIM(RTRIM(SUBSTRING(@attributeValue, CHARINDEX(':', @attributeValue) + 1, @styleIndex - CHARINDEX(':', @attributeValue))))
                                IF SUBSTRING(REVERSE(@styleValue), 1, 1) = ';'
                                    SET @styleValue = LTRIM(RTRIM(SUBSTRING(@styleValue, 1, LEN(@styleValue) - 1)))

                                INSERT INTO @Styles (NodeId, Name, Value) VALUES (@foundNodeId, @styleName, @styleValue)
                                IF @styleIndex = LEN(@attributeValue)
                                    SET @styleIndex = 0
                                ELSE
                                BEGIN
                                    SET @attributeValue = SUBSTRING(@attributeValue, @styleIndex + 1, LEN(@attributeValue) - @styleIndex)
                                    SET @styleIndex = CHARINDEX(';', @attributeValue);
                                END
                            END
                            -- parse the style end
                        END
                        ELSE
                        BEGIN
                            IF SUBSTRING(@attributeValue, 1, 1) = '"' OR SUBSTRING(@attributeValue, 1, 1) = ''''
                                SET @attributeValue = SUBSTRING(@attributeValue, 1, LEN(@attributeValue) - 1)
                            IF SUBSTRING(REVERSE(@attributeValue), 1, 1) = '"' OR SUBSTRING(REVERSE(@attributeValue), 1, 1) = ''''
                                SET @attributeValue = SUBSTRING(@attributeValue, 1, LEN(@attributeValue) - 1)

                            INSERT INTO @Attributes (NodeId, Name, Value) VALUES (@foundNodeId, @attribute, @attributeValue)
                        END
                        SET @attributeIndex = CHARINDEX('=', @attributeInfo);
                    END
                    
                    -- parse the attributes end
                END
                -- parse the nodes end
                
                SET @input = SUBSTRING(@input, LEN(@nodeDesc) + 1, LEN(@input) - LEN(@nodeDesc));
                SET @startIndex = CHARINDEX('<', @input)
                        
                IF @startIndex > 0
                BEGIN
                    SET @text = SUBSTRING(@input, 1, @startIndex - 1)
                    SET @input = SUBSTRING(@input, @startIndex, LEN(@input) - @startIndex + 1)
                    SET @startIndex = 1
                END
                ELSE
                BEGIN
                    SET @startIndex = 0			
                    SET @text = @input
                END

                IF ISNULL(@text, '') <> '' 
                BEGIN
                    INSERT INTO @Nodes (Name, Text, Integrity) VALUES ('#text', @text, 1);
                    SET @text = ''
                END
            END     --IF
            SET @startIndex = CHARINDEX('<', @input)
        END     --IF
    END     --WHILE
    
    -- Get all nodes, attributes, styles through parsing the input
    SELECT node.*, attr.Name [AttrName], attr.Value [AttrValue], style.Name [StyleName], style.Value [StyleValue] FROM @Nodes node
    LEFT JOIN @Attributes attr ON node.Id = attr.nodeId
    LEFT JOIN @Styles style ON node.Id = style.nodeId
    order by node.Id

    IF @nodePath <> '' 
    BEGIN
        IF CHARINDEX('[ERROR]', @nodePath) <> 1
            PRINT '[ERROR] ' + REPLACE(@ERR_NODE_NOT_MATCH, '{0}', CONCAT('<', REPLACE(SUBSTRING(@nodePath, 2, LEN(@nodePath) - 1), ',', '>, <'), '>'))
        ELSE
            PRINT @nodePath
    END
END