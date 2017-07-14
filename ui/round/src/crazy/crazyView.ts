import * as round from '../round';
import { drag } from './crazyCtrl';
import { game } from 'game';

import { h } from 'snabbdom'

const eventNames = ['mousedown', 'touchstart'];
const pieceRoles = ['pawn', 'knight', 'bishop', 'rook', 'queen'];

export default function pocket(ctrl, color, position) {
  var step = round.plyStep(ctrl.data, ctrl.vm.ply);
  if (!step.crazy) return;
  var droppedRole = ctrl.vm.justDropped;
  var preDropRole = ctrl.vm.preDrop;
  var pocket = step.crazy.pockets[color === 'white' ? 0 : 1];
  var usablePos = position === (ctrl.vm.flip ? 'top' : 'bottom');
  var usable = usablePos && !ctrl.replaying() && game.isPlayerPlaying(ctrl.data);
  var activeColor = color === ctrl.data.player.color;
  var captured = ctrl.vm.justCaptured;
  if (captured) captured = captured.promoted ? 'pawn' : captured.role;
  return h('div.pocket.is2d.' + position, {
    class: { usable },
    hook: {
      insert: vnode => {
        eventNames.forEach(name => {
          (vnode.elm as HTMLElement).addEventListener(name, (e: any) => {
            if (!e.shiftKey && position === (ctrl.vm.flip ? 'top' : 'bottom')) drag(ctrl, e);
          })
        });
      }
    }
  }, pieceRoles.map(role => {
    var nb = pocket[role] || 0;
    if (activeColor) {
      if (droppedRole === role) nb--;
      if (captured === role) nb++;
    }
    return h('piece.' + role + '.' + color, {
      class: { premove: activeColor && preDropRole === role },
      attrs: {
        'data-role': role,
        'data-color': color,
        'data-nb': nb,
      },
      hook: {
        insert: vnode => {
          if (ctrl.bugController && !ctrl.parent){
            var htmlElm = (vnode.elm as HTMLElement);
              htmlElm.addEventListener('dblclick', (e) => {
                (ctrl.data.player.color === color) ? ctrl.requestPiece(role) : ctrl.forbidPiece(role);
                $(e.currentTarget).addClass('blink');
              });
              htmlElm.addEventListener('click', (e: any) => {
                if (e.shiftKey){
                  (ctrl.data.player.color === color) ? ctrl.requestPiece(role) : ctrl.forbidPiece(role);
                  $(e.currentTarget).addClass('blink');
                }
              });
          'webkitAnimationEnd mozAnimationEnd oAnimationEnd oanimationend animationend'.split(' ').forEach(function(event){
                htmlElm.addEventListener(
                  event,
                  (e) => {
                    $(e.currentTarget).removeClass('blink');
                  }
                );
            })
          }
        }
      }
    });
  }));
};
