INTERFACE zif_diff3 PUBLIC.

  TYPES: number TYPE i.

  TYPES: numbers TYPE STANDARD TABLE OF number WITH EMPTY KEY.

  TYPES: number_number TYPE STANDARD TABLE OF numbers WITH EMPTY KEY.

  TYPES:
    BEGIN OF ilcsresult,
      key          TYPE i,
      buffer1index TYPE number,
      buffer2index TYPE number,
      chain        TYPE i, " ref to ilcsresult-key
    END OF ilcsresult,
    ilcsresult_t TYPE SORTED TABLE OF ilcsresult WITH UNIQUE KEY key.

**
* Expects two arrays, finds longest common sequence
* @param {string_table} buffer1
* @param {string_table} buffer2
* @returns {ILCSResult}
* @constructor
*/
  METHODS lcs
    IMPORTING
      buffer1       TYPE string_table
      buffer2       TYPE string_table
    RETURNING
      VALUE(result) TYPE ilcsresult_t.

  TYPES:
    BEGIN OF icommresult,
      common TYPE string_table,
      BEGIN OF diff,
        buffer1 TYPE string_table,
        buffer2 TYPE string_table,
      END OF diff,
    END OF icommresult,
    icommresult_t TYPE STANDARD TABLE OF icommresult WITH DEFAULT KEY.

**
* We apply the LCS to build a 'comm'-style picture of the
* differences between buffer1 and buffer2.
* @param {string_table} buffer1
* @param {string_table} buffer2
* @returns {Array<ICommResult<T>>}
*/
  METHODS diffcomm
    IMPORTING
      buffer1       TYPE string_table
      buffer2       TYPE string_table
    RETURNING
      VALUE(result) TYPE icommresult_t.

*export interface IDiffIndicesResult<T> {
*  buffer1: [number, number].
*  buffer1Content: string_table.
*  buffer2: [number, number].
*  buffer2Content: string_table.
*}
*
***
* * We apply the LCS to give a simple representation of the
* * offsets and lengths of mismatched chunks in the input
* * buffers. This is used by diff3MergeRegions.
* * @param {string_table} buffer1
* * @param {string_table} buffer2
* * @returns {IDiffIndicesResult<T>[]}
* */
*export function diffIndices<T>(
*  buffer1: string_table,
*  buffer2: string_table
*): IDiffIndicesResult<T>[].
*
*export interface IChunk<T> {
*  offset: number.
*  length: number.
*  chunk: string_table.
*}
*
*export interface IPatchRes<T> {
*  buffer1: IChunk<T>.
*  buffer2: IChunk<T>.
*}
*
***
* * We apply the LCS to build a JSON representation of a
* * diff(1)-style patch.
* * @param {string_table} buffer1
* * @param {string_table} buffer2
* * @returns {IPatchRes<T>[]}
* */
*export function diffPatch<T>(buffer1: string_table, buffer2: string_table): IPatchRes<T>[].
*
*export function patch<T>(buffer: string_table, patch: IPatchRes<T>[]): string_table.
*
*export interface IStableRegion<T> {
*  stable: true.
*  buffer: 'a' | 'o' | 'b'.
*  bufferStart: number.
*  bufferLength: number.
*  bufferContent: string_table.
*}
*
*export interface IUnstableRegion<T> {
*  stable: false.
*  aStart: number.
*  aLength: number.
*  aContent: string_table.
*  bStart: number.
*  bLength: number.
*  bContent: string_table.
*  oStart: number.
*  oLength: number.
*  oContent: string_table.
*}
*
*export type IRegion<T> = IStableRegion<T> | IUnstableRegion<T>.
*
***
* * Given three buffers, A, O, and B, where both A and B are
* * independently derived from O, returns a fairly complicated
* * internal representation of merge decisions it's taken. The
* * interested reader may wish to consult
* *
* * Sanjeev Khanna, Keshav Kunal, and Benjamin C. Pierce.
* * 'A Formal Investigation of ' In Arvind and Prasad,
* * editors, Foundations of Software Technology and Theoretical
* * Computer Science (FSTTCS), December 2007.
* *
* * (http://www.cis.upenn.edu/~bcpierce/papers/diff3-short.pdf)
* *
* * @param {string_table} a
* * @param {string_table} o
* * @param {string_table} b
* * @returns {IRegion<T>[]}
* */
*export function diff3MergeRegions<T>(a: string_table, o: string_table, b: string_table): IRegion<T>[].
*
*export interface MergeRegion<T> {
*  ok?: string_table.
*  conflict?: {
*    a: string_table.
*    aIndex: number.
*    b: string_table.
*    bIndex: number.
*    o: string_table.
*    oIndex: number.
*  }.
*}
*
*export interface MergeResult {
*  conflict: boolean.
*  result: string[].
*}
*
*export interface IMergeOptions {
*  excludeFalseConflicts?: boolean.
*  stringSeparator?: string | RegExp.
*}
*
***
* * Applies the output of diff3MergeRegions to actually
* * construct the merged buffer. the returned result alternates
* * between 'ok' and 'conflict' blocks.
* * A "false conflict" is where `a` and `b` both change the same from `o`
* *
* * @param {string | string_table} a
* * @param {string | string_table} o
* * @param {string | string_table} b
* * @param {{excludeFalseConflicts: boolean. stringSeparator: RegExp}} options
* * @returns {MergeRegion<T>[]}
* */
*export function diff3Merge<T>(
*  a: string | string_table,
*  o: string | string_table,
*  b: string | string_table,
*  options?: IMergeOptions
*): MergeRegion<T>[].
*
*export function merge<T>(
*  a: string | string_table,
*  o: string | string_table,
*  b: string | string_table,
*  options?: IMergeOptions
*): MergeResult.
*
*export function mergeDiff3<T>(
*  a: string | string_table,
*  o: string | string_table,
*  b: string | string_table,
*  options?: IMergeOptions & {
*    label?: {
*      a?: string.
*      o?: string.
*      b?: string.
*    }
*  }
*): MergeResult.
*
*export function mergeDigIn<T>(
*  a: string | string_table,
*  o: string | string_table,
*  b: string | string_table,
*  options?: IMergeOptions
*): MergeResult.

ENDINTERFACE.
