IF (SCHEMA_ID('Math') IS NULL) EXEC('CREATE SCHEMA [Math];');
GO

-------------------------------------------------------------------------------
---
--- Helper function
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.GENERATE_SERIES_bigint(
    @lower_bound bigint,
    @upper_bound bigint,
    @increment bigint)
RETURNS TABLE
AS

RETURN (
    WITH n1(n) AS (
        SELECT NULL
        FROM (VALUES (0), (0), (0), (0), (0), (0), (0), (0), (0), (0)) AS x(n)
    ),

    n2(n) AS (
        SELECT TOP (1+(@upper_bound-@lower_bound)/@increment) NULL
        FROM n1 AS a, n1 AS b, n1 AS c, n1 AS d, n1 AS e, n1 AS f
    ),

    n3(n) AS (
        SELECT TOP (1+(@upper_bound-@lower_bound)/@increment) NULL
        FROM n2 AS a, n2 AS b, n2 AS c, n2 AS d, n2 AS e, n2 AS f
    )

    SELECT [value]
    FROM (
        SELECT @lower_bound+(ROW_NUMBER() OVER (ORDER BY (SELECT NULL))-1)*@increment AS [value]
        FROM n3
        ) AS n
    WHERE [value]<=@upper_bound);

GO

-------------------------------------------------------------------------------
---
--- List primes
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Primes(@upperBound bigint)
RETURNS TABLE
AS

RETURN (
    --- The only prime evenly divisible by two
    SELECT ISNULL(CAST(2 AS bigint), 0) AS prime
    WHERE @upperBound>=2

    UNION ALL

    --- Loop through all the primes from 3, 5, 7, ...
    SELECT ISNULL(a.[value], 0) AS prime
    FROM Math.GENERATE_SERIES_bigint(CAST(3 AS bigint), @upperBound, CAST(2 AS bigint)) AS a
    --- ... except numbers that are evenly divisible by a number between a and sqrt(a).
    OUTER APPLY (
        SELECT TOP (1) 0 AS x
        FROM Math.GENERATE_SERIES_bigint(CAST(3 AS bigint), CAST(CEILING(SQRT(a.[value])) AS bigint), CAST(1 AS bigint)) AS b
        WHERE a.[value]%b.[value]=0
        ) AS x
    WHERE x.x IS NULL
    );

GO

-------------------------------------------------------------------------------
---
--- Compute the prime factors of a given integer
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Prime_factors(@integer bigint)
RETURNS TABLE
AS

RETURN (
    WITH rcte AS (
        SELECT CAST(1 AS bigint) AS prime,
               CAST(@integer AS bigint) AS remain
        --- Keeping it real:
        WHERE @integer>0

        UNION ALL

        SELECT p.prime,
               rcte.remain/p.prime AS remain
        FROM rcte
        CROSS APPLY (
            SELECT prime
            FROM (
                --- The ROW_NUMBER() pattern is a workaround because recursive
                --- common table expressions cannot contain TOP()
                SELECT p.prime, ROW_NUMBER() OVER (ORDER BY p.prime) AS _rn
                FROM Math.Primes(rcte.remain) AS p
                --- Only look at primes that are equal to or larger than
                --- the ones we've already found:
                WHERE p.prime>=rcte.prime
                  AND rcte.remain%p.prime=0
                ) AS x
            WHERE _rn=1
            ) AS p
        --- Continue the recursion until we've exhausted all the
        --- prime factors:
        WHERE rcte.remain!=1)

    SELECT prime
    FROM rcte
    WHERE prime>1);

GO

-------------------------------------------------------------------------------
---
--- Compute the greatest common divisor using the Euclidean algorithm.
---
--- https://en.wikipedia.org/wiki/Euclidean_algorithm#Implementations
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Greatest_common_divisor(@a bigint, @b bigint)
RETURNS bigint
AS

BEGIN;

    DECLARE @res bigint;

    WITH cte AS (
        SELECT (CASE WHEN @a>@b THEN @a ELSE @b END) AS a,
               (CASE WHEN @a<@b THEN @a ELSE @b END) AS b

        UNION ALL

        SELECT b AS a, a%b AS b
        FROM cte
        WHERE b!=0
    )

    SELECT @res=a
    FROM cte
    WHERE b=0;

    RETURN @res;

END;

GO

-------------------------------------------------------------------------------
---
--- Compute the least common multiple (lcm) using the greatest common divisor.
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Least_common_multiple(@a bigint, @b bigint)
RETURNS bigint
AS

BEGIN;
    RETURN (
        SELECT ABS(@a*@b)/Math.Greatest_common_divisor(@a, @b)
    );
END;

GO

-------------------------------------------------------------------------------
---
--- Compute the least common multiple (lcm) using prime factorization
---
--- The least common multiplier is the product of the distinct primes that
--- make up each of the integers. For example, the integers 10, 12, 18, can
--- be factored into (2*5), (3*4), (3*3*3), so the distinct list of primes
--- is 2, 3, 4 and 5. Thus, the lcm is 2*3*4*5 = 120.
---
--- To compute the product of a series of numbers, we're using
--- EXP(SUM(LOG(p))) as a way to leverage SQL Server's SUM() aggregate.
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Least_common_multiple_using_primes(@integer_list varchar(max))
RETURNS bigint
AS

BEGIN;
    RETURN (
        --- https://en.wikipedia.org/wiki/Least_common_multiple#Using_prime_factorization
        SELECT CAST(ROUND(EXP(SUM(DISTINCT LOG(POWER(prime, [count])))), 0) AS bigint)
        FROM (
            SELECT TOP (1) WITH TIES p.prime, COUNT(*) AS [count]
            FROM (
                SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS ordinal, [value]
                FROM STRING_SPLIT(REPLACE(@integer_list, ' ', ''), ',')
            ) AS ss
            CROSS APPLY Math.Prime_factors(CAST(ss.[value] AS bigint)) AS p
            GROUP BY ss.ordinal, p.prime
            ORDER BY ROW_NUMBER() OVER (PARTITION BY p.prime ORDER BY COUNT(*) DESC)
        ) AS x
    );
END;

GO

-------------------------------------------------------------------------------
---
--- Compute the (real) roots of a quadratic polynomial
--- by "completing the square".
---
--- y(x) = @a * x2 + @b * x + @c
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Quadratic_roots(
    @a numeric(38, 18),
    @b numeric(38, 18),
    @c numeric(38, 18))
RETURNS TABLE
WITH SCHEMABINDING
AS

RETURN (
    SELECT x
    FROM (
        SELECT -(@b+SQRT(POWER(@b, 2)-4*@a*@c)*m.multiplier)
            --- Check: @a cannot be zero:
            /NULLIF(@a*2, 0) AS x
        FROM (
            VALUES (-1), (1)
            ) AS m(multiplier)
        --- Check: SQRT() requires a non-negative value:
        WHERE POWER(@b, 2)>=4*@a*@c
        ) AS calc
    --- Trust but verify:
    WHERE @a*POWER(x, 2)+@b*x+@c=0);

GO
/*
SELECT x, x*x+x*6+5 AS y FROM Math.Quadratic_roots(1, 6, 5);
SELECT *, x*x*2+x*7+6 FROM Math.Quadratic_roots(2, 7, 6);
*/
GO

-------------------------------------------------------------------------------
---
--- Returns n!, i.e. 1*2*3*...*(n-1)*n
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Factorial(@n tinyint)
RETURNS numeric(38, 0)
AS

BEGIN;
    RETURN (
        SELECT CAST(ROUND(EXP(SUM(LOG(CAST([value] AS numeric(38, 18))))), 0) AS numeric(38, 0))
        FROM Math.GENERATE_SERIES_bigint(CAST(1 AS tinyint), @n, CAST(1 AS tinyint))
    );
END;

GO

-------------------------------------------------------------------------------
---
--- Computes n over k
---
--- See: https://en.wikipedia.org/wiki/Binomial_coefficient
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.nCk(@n int, @k int)
RETURNS numeric(38, 0)
AS

BEGIN;
    RETURN (
        SELECT (CASE WHEN @n=@K THEN CAST(1 AS numeric(38, 0))
                     ELSE Math.Factorial(@n)/(Math.Factorial(@k)*Math.Factorial(@n-@k)) END)
        WHERE @n>=@k
    );
END;

GO
/*


SELECT Math.nCk(5, 2)
SELECT (5.*4)/(2.)

SELECT Math.nCk(4, 2)
SELECT (4.*3)/(2.*1)

SELECT Math.nCk(8, 5)
SELECT (8.*7*6*5*4)/(5.*4*3*2*1),
       (8.*7*6*5*4)/(5.*4*3*2*1)
*/
GO

-------------------------------------------------------------------------------
---
--- Computes POWER(@base, @exp)%@mod for large numbers
---
--- Adapted from: https://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Modular_Exponentiation(@base numeric(38, 0), @exp bigint, @mod bigint)
RETURNS bigint
AS

BEGIN;
    DECLARE @res bigint;

    WITH iteration AS (
        SELECT CAST(@base%@mod AS numeric(38, 0)) AS b, @exp AS e, CAST(1 AS bigint) AS res
        UNION ALL
        SELECT CAST(b*b%@mod AS numeric(38, 0)),
               e/2 AS e,
               CAST((CASE WHEN e%2=1 THEN (res*b)%@mod ELSE res END) AS bigint) AS res
        FROM iteration
        WHERE e>0)

    SELECT @res=res
    FROM iteration
    WHERE e=0;

    RETURN @res;
END;

GO

-------------------------------------------------------------------------------
---
--- Computes the nth root of a number using logarithms
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.nth_Root(@number numeric(38, 8), @n smallint)
RETURNS numeric(38, 18)
AS

BEGIN;

    RETURN POWER(10., (1./@n)*LOG10(@number));

END;

GO

-------------------------------------------------------------------------------
---
--- Computes the modular multiplicative inverse,
--- https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
---
--- Adapted from
--- https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Pseudocode
---
-------------------------------------------------------------------------------

CREATE OR ALTER FUNCTION Math.Modular_multiplicative_inverse(@a bigint, @b bigint)
RETURNS bigint
AS

BEGIN;
    DECLARE @result bigint;

    WITH recursive_cte AS (
        SELECT CAST(0 AS bigint) AS t, CAST(1 AS bigint) AS new_t, @b AS r, @a AS new_r

        UNION ALL

        SELECT new_t AS t, t-quotient*new_t AS new_t,
               new_r AS r, r-quotient*new_r AS new_r
        FROM (
            SELECT t, new_t,
                   r, new_r,
                   CAST(r/new_r AS bigint) AS quotient
            FROM recursive_cte
            WHERE new_r!=0) AS sub)
        
    SELECT @result=(CASE WHEN t<0 THEN t+@b ELSE t END)
    FROM recursive_cte
    WHERE new_r=0;

    RETURN @result;
END;

GO
