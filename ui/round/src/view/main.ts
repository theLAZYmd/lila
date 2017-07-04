import { game } from 'game';
import round = require('../round');
import table = require('./table');
import promotion = require('../promotion');
import ground = require('../ground');
import { read as fenRead } from 'chessground/fen';
import util = require('../util');
import blind = require('../blind');
import keyboard = require('../keyboard');
import crazyView from '../crazy/crazyView';
import { render as keyboardMove } from '../keyboardMove';

import { h } from 'snabbdom'
import { VNode } from 'snabbdom/vnode'

function renderMaterial(material, checks, score) {
  var children: VNode[] = [];
  if (score || score === 0)
    children.push(h('score', score > 0 ? '+' + score : score));
  for (var role in material) {
    const content: VNode[] = [];
    for (var i = 0; i < material[role]; i++) content.push(h('mono-piece.' + role));
    children.push(h('tomb', content));
  }
  for (var i = 0; i < checks; i++) {
    children.push(h('tomb', h('mono-piece.king')));
  }
  return h('div.cemetery', children);
}

function wheel(ctrl, e) {
  if (game.isPlayerPlaying(ctrl.data)) return true;
  e.preventDefault();
  if (e.deltaY > 0) keyboard.next(ctrl);
  else if (e.deltaY < 0) keyboard.prev(ctrl);
  ctrl.redraw();
  return false;
}

function visualBoard(ctrl) {
  return h('div.lichess_board_wrap', [
    h('div.lichess_board.' + ctrl.data.game.variant.key + (ctrl.data.pref.blindfold ? '.blindfold' : ''), {
      hook: util.bind('wheel', e => wheel(ctrl, e))
    }, [ground.render(ctrl)]),
    promotion.view(ctrl)
  ]);
}

function blindBoard(ctrl) {
  return h('div.lichess_board_blind', [
    h('div.textual', {
      hook: {
        insert: vnode => blind.init(vnode.elm, ctrl)
      }
    }, [ ground.render(ctrl) ])
  ]);
}

var emptyMaterialDiff = {
  white: [],
  black: []
};

export function main(ctrl: any): VNode {
  const d = ctrl.data,
  cgState = ctrl.chessground && ctrl.chessground.state,
  topColor = d[ctrl.vm.flip ? 'player' : 'opponent'].color,
  bottomColor = d[ctrl.vm.flip ? 'opponent' : 'player'].color;
  let material, score;
  if (d.pref.showCaptured) {
    var pieces = cgState ? cgState.pieces : fenRead(round.plyStep(ctrl.data, ctrl.vm.ply).fen);
    material = util.getMaterialDiff(pieces);
    score = util.getScore(pieces) * (bottomColor === 'white' ? 1 : -1);
  } else material = emptyMaterialDiff;

  var boards = [
    d.blind ? blindBoard(ctrl) : visualBoard(ctrl),
    h('div.lichess_ground', [
      crazyView(ctrl, topColor, 'top') || renderMaterial(material[topColor], d.player.checks, undefined),
      table.render(ctrl),
      crazyView(ctrl, bottomColor, 'bottom') || renderMaterial(material[bottomColor], d.opponent.checks, score)
    ])
  ];

  if (ctrl.bugController){
    const bd = ctrl.bugController.data, bc = ctrl.bugController,
    bugCgState = bc.chessground && bc.chessground.state,    
    bugTopColor = bd[bc.vm.flip ? 'player' : 'opponent'].color,
    bugBottomColor = bd[bc.vm.flip ? 'opponent' : 'player'].color;
    let bugMaterial, bugScore;

    if (d.pref.showCaptured) {
      var pieces = bugCgState ? bugCgState.pieces : fenRead(round.plyStep(bd, bc.vm.ply).fen);
      bugMaterial = util.getMaterialDiff(pieces);
      bugScore = util.getScore(pieces) * (bugBottomColor === 'white' ? 1 : -1);
    }
    else bugMaterial = emptyMaterialDiff;

    boards.push(
      bd.blind ? blindBoard(bc) : visualBoard(bc),
      h('div.lichess_ground', [
        crazyView(bc, bugTopColor, 'top') || renderMaterial(bugMaterial[bugTopColor], bd.player.checks, undefined),
        table.render(bc),
        crazyView(bc, bugBottomColor, 'bottom') || renderMaterial(bugMaterial[bugBottomColor], bd.opponent.checks, bugScore)
      ])
    )
  
  $('body > .content').css("margin-left", 'calc(50% - ' + '830px' + ')');
  }
    
  return h('div.round.cg-512', [
    h('div.lichess_game.variant_' + d.game.variant.key, {
      hook: {
        insert: () => window.lichess.pubsub.emit('content_loaded')()
      }
    }, boards),
    h('div.underboard', [
      h('div.center', {
        hook: {
          insert: vnode => {
            if (ctrl.opts.crosstableEl) {
              const el = (vnode.elm as HTMLElement);
              el.insertBefore(ctrl.opts.crosstableEl, el.firstChild);
            }
          }
        }
      }, [
        ctrl.keyboardMove ? keyboardMove(ctrl.keyboardMove) : null
      ])
    ])
  ]);
};
