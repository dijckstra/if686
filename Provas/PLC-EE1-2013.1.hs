data Dica = Filme String String Int -- Nome, diretor, ano de lançamento
          | Musica String String Int String -- Artista, álbum, número da faixa, nome da faixa
		  | Jogo String Int String-- Nome, ano de lançamento, nome do publisher
		  | Relacionamento [Dicas]
		  deriving (Show, Eq)