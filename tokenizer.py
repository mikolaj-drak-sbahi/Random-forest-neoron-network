class Tokenizer:
    def __init__(self, expression):
        self.expression = expression
        self.position = 0
        self.tokens = []

    def tokenize(self):
        while self.position < len(self.expression):
            char = self.expression[self.position]

            if char.isdigit():
                # Rozpoznawanie sekwencji cyfr
                start = self.position
                while self.position < len(self.expression) and self.expression[self.position].isdigit():
                    self.position += 1

                # Sprawdzenie, czy po cyfrach występuje kropka, co oznacza liczbę rzeczywistą
                if self.position < len(self.expression) and self.expression[self.position] == '.':
                    self.position += 1

                    # Sprawdzenie, czy liczba rzeczywista zaczyna się od kropki
                    while self.position < len(self.expression) and self.expression[self.position].isdigit():
                        self.position += 1

                    end = self.position - 1
                    number_sequence = self.expression[start:end + 1]
                else:
                    end = self.position - 1
                    number_sequence = self.expression[start:end + 1]

                self.tokens.append({'character': number_sequence, 'type': 'number_real' if '.' in number_sequence else 'number_int', 'begin': start, 'end': end})

            elif char.isalpha() or char == '_':
                # Rozpoznawanie identyfikatorów
                start = self.position
                while self.position < len(self.expression) and (self.expression[self.position].isalnum() or self.expression[self.position] == '_'):
                    self.position += 1
                end = self.position - 1
                identifier = self.expression[start:end + 1]
                self.tokens.append({'character': identifier, 'type': 'identifier', 'begin': start, 'end': end})

            elif char.isspace():
                self.position += 1

            elif char in {'+', '-', '*', '/', '(', ')'}:
                self.tokens.append({'character': char, 'type': f'operator_{char}', 'begin': self.position, 'end': self.position})
                self.position += 1

            else:
                raise ValueError(f"Unexpected character at position {self.position}: {char}")

        return self.tokens

# Umożliwienie użytkownikowi wprowadzania wyrażenia
user_expression = input("Wprowadź wyrażenie: ")
tokenizer = Tokenizer(user_expression)
tokens = tokenizer.tokenize()

# Wypisanie tokenów w nowych liniach
for token in tokens:
    print(token)
