{-# LANGUAGE OverloadedStrings #-}

module Graphics.SGCDemo.Forum ( forumObj
                              , forumMtl ) where

import           Data.Monoid ( (<>) )
import           Data.Text  ( Text )
import           Data.ByteString    as BS ( ByteString )

forumObj :: [Text]
forumObj = [
    "# Blender v2.79 (sub 0) OBJ File: 'forum.blend'\n# www.blender.org\nmtllib forum.mtl\no Plane.001\nv 9.124895 1.843176 -2.127464\nv 8.846635 6.106629 -2.126796\nv 9.135159 1.843177 2.145047\nv 8.856899 6.106629 2.145715\nv 9.859984 1.891153 -2.129230\nv 9.550533 6.846906 -2.128488\nv 9.870249 1.891154 2.143281\nv 9.591979 6.841442 2.140462\nvn -0.9979 -0.0651 0.0024\nvn 0.9984 0.0561 -0.0024\nvn 0.0062 0.0009 1.0000\nvn -0.0024 -0.0000 -1.0000\nvn -0.7247 0.6891 0.0017\nvn 0.0651 -0.9979 -0.0002\nvn 0.9980 0.0623 -0.0096\nvn 0.0024 0.0000 1.0000\nvn -0.7069 0.7072 0.0078\nusemtl None\ns off\nf 3//1 2//1 1//1\nf 5//2 8//2 7//2\nf 4//3 7//3 8//3\nf 2//4 5//4 1//4\nf 4//5 6//5 2//5\nf 1//6 7//6 3//6\nf 3//1 4//1 2//1\nf 5//7 6//7 8//7\nf 4//8 3//8 7//8\nf 2//4 6//4 5//4\nf 4//9 8//9 6//9\nf 1//6 5//6 7//6\no Cylinder\nv 0.008658 -2.481924 -0.840000\nv 0.008658 2.517520 -0.840000\nv 0.173376 -2.481924 -0.823859\nv 0.173376 2.517520 -0.823859\nv 0.331764 -2.481924 -0.776059\nv 0.331764 2.517520 -0.776059\nv 0.477736 -2.481924 -0.698434\nv 0.477736 2.517520 -0.698434\nv 0.605681 -2.481924 -0.593969\nv 0.605681 2.517520 -0.593969\nv 0.710683 -2.481924 -0.466679\nv 0.710683 2.517520 -0.466679\nv 0.788706 -2.481924 -0.321454\nv 0.788706 2.517520 -0.321454\nv 0.836753 -2.481924 -0.163876\nv 0.836753 2.517520 -0.163876\nv 0.852976 -2.481924 0.000000\nv 0.852976 2.517520 0.000000\nv 0.836753 -2.481924 0.163876\nv 0.836753 2.517520 0.163876\nv 0.788706 -2.481924 0.321454\nv 0.788706 2.517520 0.321454\nv 0.710683 -2.481924 0.466679\nv 0.710683 2.517520 0.466679\nv 0.605681 -2.481924 0.593970\nv 0.605681 2.517520 0.593970\nv 0.477736 -2.481924 0.698435\nv 0.477736 2.517520 0.698435\nv 0.331764 -2.481924 0.776059\nv 0.331764 2.517520 0.776059\nv 0.173376 -2.481924 0.823860\nv 0.173376 2.517520 0.823860\nv 0.008657 -2.481924 0.840000\nv 0.008657 2.517520 0.840000\nv -0.156061 -2.481924 0.823860\nv -0.156061 2.517520 0.823860\nv -0.314449 -2.481924 0.776059\nv -0.314449 2.517520 0.776059\nv -0.460421 -2.481924 0.698434\nv -0.460421 2.517520 0.698434\nv -0.588366 -2.481924 0.593970\nv -0.588366 2.517520 0.593970\nv -0.693368 -2.481924 0.466679\nv -0.693368 2.517520 0.466679\nv -0.771391 -2.481924 0.321454\nv -0.771391 2.517520 0.321454\nv -0.819437 -2.481924 0.163875\nv -0.819437 2.517520 0.163875\nv -0.835661 -2.481924 -0.000001\nv -0.835661 2.517520 -0.000001\nv -0.819437 -2.481924 -0.163877\nv -0.819437 2.517520 -0.163877\nv -0.771390 -2.481924 -0.321455\nv -0.771390 2.517520 -0.321455\nv -0.693367 -2.481924 -0.466680\nv -0.693367 2.517520 -0.466680\nv -0.588365 -2.481924 -0.593970\nv -0.588365 2.517520 -0.593970\nv -0.460420 -2.481924 -0.698435\nv -0.460420 2.517520 -0.698435\nv -0.314448 -2.481924 -0.776059\nv -0.314448 2.517520 -0.776059\nv -0.156059 -2.481924 -0.823860\nv -0.156059 2.517520 -0.823860\nv -0.693367 0.178362 -0.466680\nv -0.588587 1.783767 -0.593701\nv -0.693368 0.178351 0.466679\nv -0.588587 1.783767 0.593701\nv -0.771390 1.966501 -0.321455\nv -0.693367 1.888507 -0.466680\nv -0.819437 2.014529 -0.163877\nv -0.835661 2.030747 -0.000001\nv -0.771391 1.966501 0.321454\nv -0.693368 1.888507 0.466679\nv -0.819437 2.014530 0.163875\nv -0.771390 -1.017094 -0.321455\nv -0.819437 -1.753254 -0.163877\nv -0.835661 -2.001826 -0.000001\nv -0.771391 -1.017103 0.321454\nv -0.819437 -1.753258 0.163875\nv 0.710683 1.367271 -0.466679\nv 0.685814 1.783767 0.496827\nv 0.710683 1.401628 0.466679\nv 0.788706 0.200831 0.321454\nv 0.788706 0.177167 -0.321454\nv 0.683420 1.783767 -0.499729\nv 0.852976 -0.795724 0.000000\nv 0.836753 -0.553187 -0.163876\nv 0.836753 -0.541124 0.163876\nv 0.852976 1.952058 0.000000\nv 0.710683 1.810940 -0.466679\nv 0.710683 1.808699 0.466679\nv 0.788706 1.887041 0.321454\nv 0.788706 1.888585 -0.321454\nv 0.836753 1.936235 -0.163876\nv 0.836753 1.935448 0.163876\nvt 0.351890 0.875000\nvt 0.704852 0.909430\nvt 0.704852 0.875000\nvt 0.351890 0.909430\nvt 0.704852 0.942537\nvt 0.351890 0.942537\nvt 0.704852 0.973048\nvt 0.351890 0.973048\nvt 0.704852 0.999791\nvt 0.351890 0.749791\nvt 0.433098 0.723048\nvt 0.704852 0.749791\nvt 0.704852 0.723048\nvt 0.517119 0.692537\nvt 0.704852 0.692537\nvt 0.568683 0.659430\nvt 0.704852 0.659430\nvt 0.585806 0.625000\nvt 0.704852 0.625000\nvt 0.704852 0.590570\nvt 0.567831 0.590570\nvt 0.704852 0.557464\nvt 0.515449 0.557464\nvt 0.704852 0.526953\nvt 0.403693 0.520618\nvt 0.351890 0.500209\nvt 0.430672 0.526953\nvt 0.351890 0.249791\nvt 0.704852 0.223048\nvt 0.704852 0.249791\nvt 0.351890 0.223048\nvt 0.704852 0.192537\nvt 0.351890 0.192537\nvt 0.704852 0.159430\nvt 0.351890 0.159430\nvt 0.704852 0.125000\nvt 0.351890 0.125000\nvt 0.704852 0.090570\nvt 0.351890 0.090570\nvt 0.704852 0.057464\nvt 0.351890 0.057464\nvt 0.704852 0.026952\nvt 0.351890 0.026952\nvt 0.704852 0.000209\nvt 0.704852 0.250209\nvt 0.351890 0.250209\nvt 0.403693 0.250265\nvt 0.704852 0.276952\nvt 0.601435 0.307464\nvt 0.704852 0.307464\nvt 0.653408 0.340570\nvt 0.704852 0.340570\nvt 0.670957 0.375000\nvt 0.704852 0.375000\nvt 0.704852 0.409430\nvt 0.653408 0.409430\nvt 0.704852 0.442537\nvt 0.601435 0.442537\nvt 0.704852 0.473048\nvt 0.403693 0.499735\nvt 0.351890 0.499791\nvt 0.704852 0.499791\nvt 0.351890 0.750209\nvt 0.704852 0.776953\nvt 0.704852 0.750209\nvt 0.351890 0.776953\nvt 0.704852 0.807464\nvt 0.006958 0.227070\nvt 0.158542 0.000209\nvt 0.020198 0.259033\nvt 0.351890 0.807464\nvt 0.704852 0.840571\nvt 0.351890 0.840571\nvt 0.193139 0.703152\nvt 0.312262 0.415562\nvt 0.158542 0.703152\nvt 0.387401 0.409430\nvt 0.351890 0.442537\nvt 0.390792 0.442537\nvt 0.351890 0.473048\nvt 0.396298 0.473048\nvt 0.387401 0.340570\nvt 0.351890 0.307464\nvt 0.351890 0.340570\nvt 0.386256 0.375000\nvt 0.351890 0.409430\nvt 0.390792 0.307464\nvt 0.351890 0.276952\nvt 0.351890 0.375000\nvt 0.718704 0.057407\nvt 0.708661 0.159373\nvt 0.718704 0.192480\nvt 0.392929 0.659430\nvt 0.351890 0.692537\nvt 0.351890 0.659430\nvt 0.396402 0.557464\nvt 0.351890 0.526953\nvt 0.401933 0.526953\nvt 0.392984 0.590570\nvt 0.351890 0.557464\nvt 0.396293 0.692537\nvt 0.351890 0.723048\nvt 0.391812 0.625000\nvt 0.351890 0.590570\nvt 0.718542 0.287717\nvt 0.708743 0.389706\nvt 0.718865 0.422789\nvt 0.351890 0.625000\nvt 0.351890 0.999791\nvt 0.401775 0.723048\nvt 0.403693 0.729992\nvt 0.704852 0.500209\nvt 0.351890 0.000209\nvt 0.396298 0.276952\nvt 0.517036 0.276952\nvt 0.517035 0.473048\nvt 0.193139 0.000209\nvt 0.227070 0.006958\nvt 0.063882 0.312262\nvt 0.259033 0.020198\nvt 0.287799 0.039418\nvt 0.124611 0.344722\nvt 0.312262 0.063882\nvt 0.331483 0.092648\nvt 0.158542 0.351472\nvt 0.344722 0.124610\nvt 0.351472 0.158542\nvt 0.227070 0.344722\nvt 0.351472 0.193138\nvt 0.344722 0.227070\nvt 0.312262 0.287799\nvt 0.331483 0.259033\nvt 0.287799 0.312262\nvt 0.259033 0.331483\nvt 0.193139 0.351472\nvt 0.092648 0.331483\nvt 0.039418 0.287799\nvt 0.000209 0.193138\nvt 0.124611 0.006958\nvt 0.000209 0.158542\nvt 0.006958 0.124610\nvt 0.039419 0.063882\nvt 0.020198 0.092648\nvt 0.063882 0.039418\nvt 0.092648 0.020198\nvt 0.287799 0.391099\nvt 0.259033 0.371879\nvt 0.092648 0.683164\nvt 0.227070 0.358639\nvt 0.193139 0.351890\nvt 0.063882 0.663943\nvt 0.158542 0.351890\nvt 0.124611 0.358639\nvt 0.006958 0.578751\nvt 0.092648 0.371878\nvt 0.063882 0.391099\nvt 0.000209 0.544819\nvt 0.039419 0.415562\nvt 0.020198 0.444328\nvt 0.006958 0.476291\nvt 0.000209 0.510223\nvt 0.020198 0.610714\nvt 0.039418 0.639480\nvt 0.124611 0.696403\nvt 0.227070 0.696403\nvt 0.344722 0.476291\nvt 0.259033 0.683164\nvt 0.287799 0.663943\nvt 0.351472 0.510223\nvt 0.312262 0.639480\nvt 0.331483 0.610714\nvt 0.344722 0.578751\nvt 0.351472 0.544819\nvt 0.331483 0.444328\nvt 0.735013 0.222991\nvt 0.756914 0.249678\nvt 0.756914 0.000209\nvt 0.735012 0.026896\nvt 0.708661 0.090514\nvt 0.705270 0.124944\nvt 0.735247 0.453261\nvt 0.740460 0.459583\nvt 0.740460 0.250209\nvt 0.734778 0.257166\nvt 0.708579 0.320847\nvt 0.705270 0.355285\nvn 0.0000 0.6858 -0.7278\nvn 0.1413 -0.6858 -0.7139\nvn 0.0000 -0.6858 -0.7278\nvn 0.1413 0.6858 -0.7139\nvn 0.2773 -0.6858 -0.6729\nvn 0.2773 0.6858 -0.6729\nvn 0.4029 -0.6857 -0.6061\nvn 0.4029 0.6857 -0.6061\nvn 0.5134 -0.6857 -0.5160\nvn 0.5134 0.6857 -0.5160\nvn 0.9400 0.0341 -0.3393\nvn 0.6043 -0.6856 -0.4058\nvn 0.9689 0.0326 -0.2452\nvn 0.6720 -0.6856 -0.2798\nvn 0.9886 0.0289 -0.1476\nvn 0.7138 -0.6856 -0.1427\nvn 0.9999 0.0121 -0.0001\nvn 0.7280 -0.6856 0.0000\nvn 0.7138 -0.6856 0.1427\nvn 0.9890 0.0291 0.1453\nvn 0.6720 -0.6856 0.2798\nvn 0.9695 0.0327 0.2429\nvn 0.6043 -0.6856 0.4058\nvn 0.8906 -0.0851 0.4468\nvn 0.5134 0.6857 0.5160\nvn 0.9409 0.0342 0.3370\nvn 0.4029 -0.6857 0.6061\nvn 0.5134 -0.6857 0.5160\nvn 0.4029 0.6857 0.6061\nvn 0.2773 -0.6858 0.6729\nvn 0.2773 0.6858 0.6729\nvn 0.1413 -0.6858 0.7139\nvn 0.1413 0.6858 0.7139\nvn 0.0000 -0.6858 0.7278\nvn 0.0000 0.6858 0.7278\nvn -0.1413 -0.6858 0.7139\nvn -0.1413 0.6858 0.7139\nvn -0.2773 -0.6858 0.6729\nvn -0.2773 0.6858 0.6729\nvn -0.4029 -0.6857 0.6061\nvn -0.4029 0.6857 0.6061\nvn -0.5134 -0.6856 0.5160\nvn -0.5134 0.6857 0.5160\nvn -0.8901 -0.0849 0.4477\nvn -0.6043 -0.6856 0.4058\nvn -0.9692 0.0326 0.2440\nvn -0.6720 -0.6856 0.2798\nvn -0.9888 0.0290 0.1465\nvn -0.7138 -0.6856 0.1427\nvn -0.9999 0.0121 0.0000\nvn -0.7280 -0.6856 0.0000\nvn -0.7138 -0.6856 -0.1427\nvn -0.9888 0.0290 -0.1465\nvn -0.6720 -0.6856 -0.2798\nvn -0.9692 0.0326 -0.2440\nvn -0.6043 -0.6856 -0.4058\nvn -0.8901 -0.0848 -0.4477\nvn -0.5134 0.6857 -0.5160\nvn -0.5134 -0.6856 -0.5160\nvn -0.4029 -0.6857 -0.6061\nvn -0.4029 0.6857 -0.6061\nvn -0.2773 -0.6858 -0.6729\nvn -0.2773 0.6858 -0.6729\nvn -0.1413 -0.6858 -0.7139\nvn -0.1413 0.6858 -0.7139\nvn -0.9262 -0.3571 -0.1205\nvn -0.6720 0.6856 -0.2798\nvn -0.9003 -0.3652 -0.2366\nvn -0.6043 0.6856 -0.4058\nvn -0.8592 -0.3776 -0.3453\nvn -0.9262 -0.3571 0.1205\nvn -0.6720 0.6856 0.2798\nvn -0.7138 0.6856 0.1427\nvn -0.9351 -0.3542 0.0000\nvn -0.7138 0.6856 -0.1427\nvn -0.9003 -0.3652 0.2366\nvn -0.6043 0.6856 0.4058\nvn -0.7280 0.6856 0.0000\nvn 0.9261 -0.3571 -0.1213\nvn 0.6720 0.6856 -0.2798\nvn 0.7138 0.6856 -0.1427\nvn 0.9005 -0.3654 0.2358\nvn 0.6043 0.6856 0.4058\nvn 0.8594 -0.3777 0.3445\nvn 0.9263 -0.3572 0.1198\nvn 0.6720 0.6856 0.2798\nvn 0.9002 -0.3651 -0.2373\nvn 0.6043 0.6856 -0.4058\nvn 0.9351 -0.3542 -0.0008\nvn 0.7138 0.6856 0.1427\nvn 0.7280 0.6856 0.0000\nvn 0.8589 -0.3774 -0.3460\nvn 0.8897 -0.0846 -0.4485\nvn -0.8592 -0.3776 0.3453\nvn -0.9404 0.0342 0.3382\nvn -0.9404 0.0342 -0.3382\nusemtl Material.004\ns 1\nf 10/1/10 11/2/11 9/3/12\nf 12/4/13 13/5/14 11/2/11\nf 14/6/15 15/7/16 13/5/14\nf 16/8/17 17/9/18 15/7/16\nf 18/10/19 89/11/20 17/12/18\nf 19/13/21 93/14/22 21/15/23\nf 21/15/23 96/16/24 23/17/25\nf 23/17/25 95/18/26 25/19/27\nf 95/18/26 27/20/28 25/19/27\nf 97/21/29 29/22/30 27/20/28\nf 92/23/31 31/24/32 29/22/30\nf 90/25/33 34/26/34 91/27/35\nf 34/28/34 35/29/36 33/30/37\nf 36/31/38 37/32/39 35/29/36\nf 38/33/40 39/34/41 37/32/39\nf 40/35/42 41/36/43 39/34/41\nf 42/37/44 43/38/45 41/36/43\nf 44/39/46 45/40/47 43/38/45\nf 46/41/48 47/42/49 45/40/47\nf 48/43/50 49/44/51 47/42/49\nf 49/45/51 50/46/52 76/47/53\nf 51/48/54 87/49/55 53/50/56\nf 53/50/56 88/51/57 55/52/58\nf 55/52/58 86/53/59 57/54/60\nf 86/53/59 59/55/61 57/54/60\nf 85/56/62 61/57/63 59/55/61\nf 84/58/64 63/59/65 61/57/63\nf 74/60/66 66/61/67 65/62/68\nf 66/63/67 67/64/69 65/65/68\nf 68/66/70 69/67/71 67/64/69\nf 34/68/34 16/69/17 36/70/38\nf 70/71/72 71/72/73 69/67/71\nf 72/73/74 9/3/12 71/72/73\nf 47/74/49 69/75/71 45/76/47\nf 79/77/75 62/78/76 77/79/77\nf 77/79/77 64/80/78 78/81/79\nf 83/82/80 54/83/81 56/84/82\nf 80/85/83 60/86/84 79/77/75\nf 81/87/85 52/88/86 54/83/81\nf 80/85/83 56/84/82 58/89/87\nf 85/56/62 88/51/57 87/49/55\nf 81/90/85 79/91/75 77/92/77\nf 103/93/88 22/94/89 24/95/90\nf 101/96/91 32/97/92 100/98/93\nf 104/99/94 30/100/95 101/96/91\nf 102/101/96 20/102/97 22/94/89\nf 98/103/98 28/104/99 104/99/94\nf 97/21/29 96/16/24 93/14/22\nf 102/105/96 104/106/94 101/107/91\nf 98/103/98 24/95/90 26/108/100\nf 10/1/10 12/4/13 11/2/11\nf 12/4/13 14/6/15 13/5/14\nf 14/6/15 16/8/17 15/7/16\nf 16/8/17 18/109/19 17/9/18\nf 19/13/21 17/12/18 89/11/20\nf 18/10/19 20/102/97 99/110/101\nf 94/111/102 89/11/20 18/10/19\nf 18/10/19 99/110/101 94/111/102\nf 19/13/21 89/11/20 93/14/22\nf 21/15/23 93/14/22 96/16/24\nf 23/17/25 96/16/24 95/18/26\nf 95/18/26 97/21/29 27/20/28\nf 97/21/29 92/23/31 29/22/30\nf 92/23/31 91/27/35 31/24/32\nf 33/112/37 31/24/32 91/27/35\nf 90/25/33 100/98/93 34/26/34\nf 33/112/37 91/27/35 34/26/34\nf 100/98/93 32/97/92 34/26/34\nf 34/28/34 36/31/38 35/29/36\nf 36/31/38 38/33/40 37/32/39\nf 38/33/40 40/35/42 39/34/41\nf 40/35/42 42/37/44 41/36/43\nf 42/37/44 44/39/46 43/38/45\nf 44/39/46 46/41/48 45/40/47\nf 46/41/48 48/43/50 47/42/49\nf 48/43/50 50/113/52 49/44/51\nf 50/46/52 52/88/86 82/114/103\nf 76/47/53 75/115/104 49/45/51\nf 50/46/52 82/114/103 76/47/53\nf 75/115/104 51/48/54 49/45/51\nf 51/48/54 75/115/104 87/49/55\nf 53/50/56 87/49/55 88/51/57\nf 55/52/58 88/51/57 86/53/59\nf 86/53/59 85/56/62 59/55/61\nf 85/56/62 84/58/64 61/57/63\nf 84/58/64 73/116/105 63/59/65\nf 65/62/68 63/59/65 73/116/105\nf 74/60/66 78/81/79 66/61/67\nf 65/62/68 73/116/105 74/60/66\nf 78/81/79 64/80/78 66/61/67\nf 66/63/67 68/66/70 67/64/69\nf 68/66/70 70/71/72 69/67/71\nf 14/117/15 12/118/13 40/119/42\nf 10/120/10 72/121/74 44/122/46\nf 70/123/72 68/124/70 46/125/48\nf 66/126/67 64/127/78 50/128/52\nf 62/129/76 60/130/84 56/131/82\nf 58/132/87 56/131/82 60/130/84\nf 54/133/81 52/134/86 62/129/76\nf 50/128/52 48/135/50 66/126/67\nf 46/125/48 44/122/46 72/121/74\nf 42/136/44 40/119/42 12/118/13\nf 38/137/40 36/70/38 14/117/15\nf 34/68/34 32/138/92 18/139/19\nf 30/140/95 28/141/99 24/142/90\nf 26/143/100 24/142/90 28/141/99\nf 22/144/89 20/145/97 30/140/95\nf 18/139/19 16/69/17 34/68/34\nf 12/118/13 10/120/10 42/136/44\nf 68/124/70 48/135/50 46/125/48\nf 62/129/76 56/131/82 54/133/81\nf 52/134/86 64/127/78 62/129/76\nf 44/122/46 42/136/44 10/120/10\nf 36/70/38 16/69/17 14/117/15\nf 30/140/95 24/142/90 22/144/89\nf 20/145/97 32/138/92 30/140/95\nf 72/121/74 70/123/72 46/125/48\nf 52/134/86 50/128/52 64/127/78\nf 40/119/42 38/137/40 14/117/15\nf 20/145/97 18/139/19 32/138/92\nf 66/126/67 48/135/50 68/124/70\nf 70/71/72 72/73/74 71/72/73\nf 72/73/74 10/1/10 9/3/12\nf 71/146/73 9/147/12 41/148/43\nf 11/149/11 13/150/14 39/151/41\nf 15/152/16 17/153/18 33/154/37\nf 19/155/21 21/156/23 31/157/32\nf 23/158/25 25/159/27 27/160/28\nf 27/160/28 29/161/30 23/158/25\nf 31/157/32 33/154/37 17/153/18\nf 35/162/36 37/163/39 15/152/16\nf 39/151/41 41/148/43 9/147/12\nf 43/164/45 45/76/47 71/146/73\nf 47/74/49 49/165/51 65/166/68\nf 51/167/54 53/168/56 63/169/65\nf 55/170/58 57/171/60 59/172/61\nf 59/172/61 61/173/63 55/170/58\nf 63/169/65 65/166/68 49/165/51\nf 67/174/69 69/75/71 47/74/49\nf 9/147/12 11/149/11 39/151/41\nf 17/153/18 19/155/21 31/157/32\nf 23/158/25 29/161/30 21/156/23\nf 33/154/37 35/162/36 15/152/16\nf 41/148/43 43/164/45 71/146/73\nf 49/165/51 51/167/54 63/169/65\nf 55/170/58 61/173/63 53/168/56\nf 65/166/68 67/174/69 47/74/49\nf 13/150/14 37/163/39 39/151/41\nf 29/161/30 31/157/32 21/156/23\nf 45/76/47 69/75/71 71/146/73\nf 61/173/63 63/169/65 53/168/56\nf 15/152/16 37/163/39 13/150/14\nf 79/77/75 60/86/84 62/78/76\nf 77/79/77 62/78/76 64/80/78\nf 83/82/80 81/87/85 54/83/81\nf 80/85/83 58/89/87 60/86/84\nf 81/87/85 82/114/103 52/88/86\nf 80/85/83 83/82/80 56/84/82\nf 75/115/104 76/47/53 73/116/105\nf 74/60/66 73/116/105 76/47/53\nf 84/58/64 85/56/62 87/49/55\nf 86/53/59 88/51/57 85/56/62\nf 87/49/55 75/115/104 84/58/64\nf 73/116/105 84/58/64 75/115/104\nf 78/175/79 74/176/66 76/177/53\nf 76/177/53 82/178/103 78/175/79\nf 81/90/85 83/179/80 79/91/75\nf 80/180/83 79/91/75 83/179/80\nf 77/92/77 78/175/79 82/178/103\nf 82/178/103 81/90/85 77/92/77\nf 103/93/88 102/101/96 22/94/89\nf 101/96/91 30/100/95 32/97/92\nf 104/99/94 28/104/99 30/100/95\nf 102/101/96 99/110/101 20/102/97\nf 98/103/98 26/108/100 28/104/99\nf 89/11/20 94/111/102 91/27/35\nf 90/25/33 91/27/35 94/111/102\nf 92/23/31 97/21/29 93/14/22\nf 95/18/26 96/16/24 97/21/29\nf 93/14/22 89/11/20 92/23/31\nf 91/27/35 92/23/31 89/11/20\nf 100/181/93 90/182/33 101/107/91\nf 94/183/102 99/184/101 102/105/96\nf 102/105/96 103/185/88 104/106/94\nf 98/186/98 104/106/94 103/185/88\nf 101/107/91 90/182/33 94/183/102\nf 94/183/102 102/105/96 101/107/91\nf 98/103/98 103/93/88 24/95/90\n"

           ]

forumMtl :: ByteString
forumMtl =
    "# Blender MTL File: 'forum.blend'\n" <>
    "# Material Count: 2\n" <>
    "\n" <>
    "newmtl Material.004\n" <>
    "Ns 96.078431\n" <>
    "Ka 1.000000 1.000000 1.000000\n" <>
    "Kd 0.640000 0.640000 0.640000\n" <>
    "Ks 0.500000 0.500000 0.500000\n" <>
    "Ke 0.000000 0.000000 0.000000\n" <>
    "Ni 1.000000\n" <>
    "d 1.000000\n" <>
    "illum 2\n" <>
    "map_d .\n" <>
    "\n" <>
    "newmtl None\n" <>
    "Ns 0\n" <>
    "Ka 0.000000 0.000000 0.000000\n" <>
    "Kd 0.8 0.8 0.8\n" <>
    "Ks 0.8 0.8 0.8\n" <>
    "d 1\n" <>
    "illum 2\n"