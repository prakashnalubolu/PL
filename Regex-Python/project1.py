import json
import sys
import re


TOKENS = {
    'NUMBER': r'(?<![\w:])\d+(?:_\d+)*',
    'ATOM': r':\w+', 
    'KEY': r'\w+:',  
    'TRUE': r'\btrue\b',  
    'FALSE': r'\bfalse\b',  
    'LIST_START': r'\[', 
    'LIST_END': r'\]',  
    'TUPLE_START': r'\{',  
    'TUPLE_END': r'\}',  
    'MAP_START': r'%\{',  
    'MAP_END': r'\}',  
    'COMMA': r',', 
    'KEY_VALUE': r'=>', 
    'WHITESPACE': r'\s+',  
    'COMMENT': r'#.*',  
}

class Lexer:
    def __init__(self, input):
        self.tokens = []
        self.current = 0
        processed_input = re.sub(TOKENS['COMMENT'], '', input)  

        for token_type, regex in TOKENS.items():
            if token_type != 'WHITESPACE':  
                for match in re.finditer(regex, processed_input):
                    self.tokens.append((token_type, match.group(), match.start()))
        self.tokens.sort(key=lambda x: x[2])

     
        self.validate_token_separation(processed_input)

    def validate_token_separation(self, input):
        last_end = 0
        for token_type, token_value, start_pos in self.tokens:
            
            if start_pos > last_end and not input[last_end:start_pos].isspace():
                raise SyntaxError(f"Invalid token concatenation detected at position {last_end}")
            last_end = start_pos + len(token_value)

        
        if last_end < len(input) and not input[last_end:].isspace():
            raise SyntaxError("Invalid trailing content detected")

    def get_next_token(self):
        if self.current < len(self.tokens):
            token = self.tokens[self.current]
            self.current += 1
            return token
        return 'EOF', '', None

def parse_data_literal(lexer):
    token_type, token_value, _ = lexer.get_next_token()
    if token_type == 'NUMBER':
        return {'%k': 'int', '%v': int(token_value.replace('_', ''))}
    elif token_type in ['TRUE', 'FALSE']:
        return {'%k': 'bool', '%v': token_value == 'true'}
    elif token_type == 'ATOM':
        return {'%k': 'atom', '%v': token_value}
    elif token_type == 'KEY':
        return {'%k': 'key', '%v': token_value[:-1]}
    elif token_type == 'LIST_START':
        return parse_list(lexer)
    elif token_type == 'TUPLE_START':
        return parse_tuple(lexer)
    elif token_type == 'MAP_START':
        return parse_map(lexer)
    return None

def parse_list(lexer):
    items = []
    last_token_type = None  
    while True:
        token_type, token_value, _ = lexer.get_next_token()

        if token_type == 'LIST_END':
            if last_token_type == 'COMMA':  
                raise SyntaxError("Trailing comma before list end")
            break  

        if token_type == 'EOF':
            raise SyntaxError("Unexpected EOF while parsing list")

        if token_type == 'COMMA':
            if last_token_type in ['LIST_START', 'COMMA']:  
                raise SyntaxError("Invalid comma usage in list")
            
            last_token_type = token_type
            continue  

        if token_type not in ['NUMBER', 'ATOM', 'TRUE', 'FALSE', 'LIST_START', 'TUPLE_START', 'MAP_START']:
            raise SyntaxError(f"Unexpected token {token_type} encountered")

        lexer.current -= 1  
        item = parse_data_literal(lexer)
        items.append(item)
        last_token_type = token_type  

    return {'%k': 'list', '%v': items}


def parse_tuple(lexer):
    items = []
    last_token_type = None  
    while True:
        token_type, token_value, _ = lexer.get_next_token()

        if token_type == 'TUPLE_END':
            if last_token_type == 'COMMA':  
                raise SyntaxError("Trailing comma before tuple end")
            break  #

        if token_type == 'EOF':
            raise SyntaxError("Unexpected EOF while parsing tuple")

        if token_type == 'COMMA':
            if last_token_type in ['TUPLE_START', 'COMMA']: 
                raise SyntaxError("Invalid comma usage in tuple")
            last_token_type = token_type
            continue

        if token_type not in ['NUMBER', 'ATOM', 'TRUE', 'FALSE', 'LIST_START', 'TUPLE_START', 'MAP_START', 'KEY']:
            raise SyntaxError(f"Unexpected token {token_type} encountered")

        lexer.current -= 1  
        item = parse_data_literal(lexer)
        items.append(item)
        last_token_type = token_type

    return {'%k': 'tuple', '%v': items}

def parse_map(lexer):
    pairs = []
    last_token_type = None
    
    while True:
        token_type, _, _ = lexer.get_next_token()
        if token_type == 'MAP_END':
            if last_token_type == 'COMMA':  
                raise SyntaxError("Trailing comma before Map end")
            return {'%k': 'map', '%v': pairs}

        if token_type == 'COMMA':
            raise SyntaxError(f"Unexpected token {token_type} encountered")

        if token_type == 'KEY':
            atom_key = ':' + token_type[:-1]  
            key = {'%k': 'atom', '%v': atom_key}
        key = parse_data_literal(lexer)

      
        sep_type, _, _ = lexer.get_next_token()
        if sep_type != 'KEY_VALUE':
            if sep_type == 'MAP_END' or sep_type =='EOF':
                break  
            else:
                raise SyntaxError(f"Expected key-value separator '=>', got {sep_type} instead")

        value = parse_data_literal(lexer)
        pairs.append([key, value])
        if token_type == 'COMMA':
            continue

    return {'%k': 'map', '%v': pairs}
        

def main():
    input_string = sys.stdin.read()
    lexer = Lexer(input_string)
    parsed_data = []
    while True:
        data_literal = parse_data_literal(lexer)
        if data_literal:
            parsed_data.append(data_literal)
        else:
            token_type, _, _ = lexer.get_next_token()
            if token_type == 'EOF':
                break  
            elif token_type in ['TUPLE_END', 'LIST_END', 'MAP_END']:
                raise SyntaxError("Unmatched closing token detected")
            lexer.current -= 1  

    print(json.dumps(parsed_data, separators=(',', ':')))

if __name__ == "__main__":
    main()
