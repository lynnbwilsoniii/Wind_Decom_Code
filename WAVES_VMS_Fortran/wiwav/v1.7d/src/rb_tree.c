/* rb_tree.c - implements a red-black binary search tree
*/

#include <stdio.h>
#include <string.h>
#include "item.h"
#define MYSTRUCT item_node
#define MYVISIT wid_show_item_node
#define RED 2
#define BLACK 3

extern void MYVISIT();

void wid_inorder_traverse2(x, myvisit)
   struct MYSTRUCT *x;
   void (*myvisit)();
{
   if (x==NULL) return;
   wid_inorder_traverse2(x->left, myvisit);
   myvisit(x);
   wid_inorder_traverse2(x->right, myvisit);
}
void wid_inorder_traverse(x)
   struct MYSTRUCT *x;
{
   if (x==NULL) return;
   wid_inorder_traverse(x->left);
   MYVISIT(x);
   wid_inorder_traverse(x->right);
}

void nth_tree_element(x, tx, target, thisone)
   struct MYSTRUCT *x;
   struct MYSTRUCT **tx;
   int target;
   int *thisone;
{
   if (x==NULL) return;
   nth_tree_element(x->left, tx, target, thisone);
   ++*thisone;
   if (target <= *thisone)
   {
      if (target == *thisone) *tx = x;
      x = NULL;
      return;
   }
   else
   nth_tree_element(x->right, tx, target, thisone);
}

struct MYSTRUCT *tree_search(x, k, depth)
   struct MYSTRUCT *x;
   char *k;
   int *depth;
{
   int i;

   (*depth)++;
   if (x==NULL) return x;
   i = strcmp(k, x->item_name);
   if (0 == i) return x;

   if (i < 0) return tree_search(x->left, k, depth);
   return tree_search(x->right, k, depth);
}

static void tree_insert(root, z)
   struct MYSTRUCT **root;
   struct MYSTRUCT *z;
{
   struct MYSTRUCT *x;
   struct MYSTRUCT *y;

   z->left = NULL;
   z->right = NULL;
   z->parent = NULL;
   y = NULL;
   x = *root;
   while (x != NULL)
   {
      y = x;
      if (strcmp(x->item_name, z->item_name) > 0 )
         x = x->left;
      else
         x = x->right;
   }

   z->parent = y;
   if (y == NULL)
      *root = z;
   else
      if (strcmp(y->item_name, z->item_name) > 0)
         y->left = z;
      else
         y->right = z;

   return;
}

/* assumes x->right != NULL */
static void left_rotate(root, x)
   struct MYSTRUCT **root;
   struct MYSTRUCT *x;
{
   struct MYSTRUCT *y;

   y = x->right;
   x->right = y->left;

   if (y->left != NULL) y->left->parent = x;
   y->parent = x->parent;

   if (x->parent == NULL)
      *root = y;
   else
      if (x==x->parent->left) x->parent->left = y;
      else x->parent->right = y;

   y->left = x;
   x->parent = y;

   return;
}
     
/* assumes x->left != NULL */
static void right_rotate(root, x)
   struct MYSTRUCT **root;
   struct MYSTRUCT *x;
{
   struct MYSTRUCT *y;

   y = x->left;
   x->left = y->right;

   if (y->right != NULL) y->right->parent = x;
   y->parent = x->parent;

   if (x->parent == NULL)
      *root = y;
   else
      if (x==x->parent->right) x->parent->right = y;
      else x->parent->left = y;

   y->right = x;
   x->parent = y;

   return;
}
     
void rb_insert(root, x)
   struct MYSTRUCT **root;
   struct MYSTRUCT *x;
{
   struct MYSTRUCT *y;
   int i;

   tree_insert(root, x);
   x->color = RED;
   i = 0;
   while(x != *root && x->parent->color == RED)
   {
/*
   while(x != *root && x->parent->color == RED && x->parent->parent != NULL)
printf("... in loop %d\n", ++i);
puts("A, NO rotate");
puts("A, left rotate");
puts("A, right rotate");
puts("B, NO rotate");
puts("B, right rotate");
puts("B, left rotate");
*/
      if (x->parent == x->parent->parent->left)
      {
         y = x->parent->parent->right;
         if (y != NULL && y->color == RED)
         {
            x->parent->color = BLACK;
            y->color = BLACK;
            x->parent->parent->color = RED;
            x = x->parent->parent;
         }
         else
         {
           if (x == x->parent->right)
           {
              x = x->parent;
              left_rotate(root, x);
           }
           x->parent->color = BLACK;
           x->parent->parent->color = RED;
           right_rotate(root, x->parent->parent);
         }
      }
      else
      {
         y = x->parent->parent->left;
         if (y != NULL && y->color == RED)
         {
            x->parent->color = BLACK;
            y->color = BLACK;
            x->parent->parent->color = RED;
            x = x->parent->parent;
         }
         else
         {
           if (x == x->parent->left)
           {
              x = x->parent;
              right_rotate(root, x);
           }
           x->parent->color = BLACK;
           x->parent->parent->color = RED;
           left_rotate(root, x->parent->parent);
         }
      }
   }
   (*root)->color = BLACK;
}
