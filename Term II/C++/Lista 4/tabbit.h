#pragma once
#include <cstdint>
#include <iostream>


class tab_bit
{
	typedef uint64_t slowo; // komorka w tablicy
	static const int rozmiarSlowa = sizeof(slowo)*8; // rozmiar slowa w bitach
	// klasa pomocnicza do adresowania bitów
	class ref {
		/* Na podstawie https://stackoverflow.com/questions/9531214/access-individual-bits-in-a-char-c
		If you want access bit N:
		Get: (INPUT >> N) & 1;
		Set: INPUT |= 1 << N;
		Unset: INPUT &= ~(1 << N);*/
	private:
		static const int rozmiarSlowa = sizeof(slowo)*8; // rozmiar slowa w bitach
		slowo * t;
		slowo i;
	public:
		ref(slowo * what, slowo index = 0);
		bool operator=(bool v);
		bool operator=(ref r);
		bool operator[](int i);
		bool set(slowo i);
		bool unset(slowo i);
		bool get(slowo i);
		operator bool();
	};
protected:
	int dl; // liczba bitów
	slowo *tab; // tablica bitów
public:
	explicit tab_bit(int rozm);				// wyzerowana tablica bitow [0...rozm]
	explicit tab_bit(slowo tb);			// tablica bitów [0...rozmiarSlowa]
											// zainicjalizowana wzorcem
	tab_bit(const tab_bit &tb);				// konstruktor kopiujacy
	tab_bit(tab_bit &&tb);				// konstruktor przenoszacy
	tab_bit(std::initializer_list<bool> il);
	tab_bit & operator = (const tab_bit &tb); // przypisanie kopiujace
	tab_bit & operator = (tab_bit &&tb);	// przypisanie przenoszace
	~tab_bit();								// destruktor
private:
	bool czytaj(int i) const;		// metoda pomocnicza do odczytu bitu
	bool pisz(int i, bool b);		// metoda pomocnicza do zapisu bitu
public:
	bool operator[] (int i) const;	// indeksowanie dla sta³ych tablic bitowych
	ref operator[] (int i);			// indeksowanie dla zwyk³ych tablic bitowych
	inline int rozmiar() const {	// rozmiar tablicy w bitach
		return dl;
	}; 
public:
	// operatory bitowe
	tab_bit operator|(const tab_bit & tb);			// operator alternatywy
	tab_bit & operator|=(const tab_bit & tb);
	tab_bit operator&(const tab_bit & tb);			// operator koniunkcji
	tab_bit & operator&=(const tab_bit & tb);
	tab_bit operator^(const tab_bit & tb);			// operator roznicy symetrycznej
	tab_bit & operator^=(const tab_bit & tb);
	friend tab_bit operator!(const tab_bit & tb);	// operator negacji
public:
	friend std::istream & operator >> (std::istream &we, tab_bit &tb);
	friend std::ostream & operator << (std::ostream &wy, const tab_bit &tb);
};